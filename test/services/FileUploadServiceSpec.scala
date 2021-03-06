/*
 * Copyright 2017 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package services

import akka.util.ByteString
import auth.{TAVCUser, ggUser}
import common.{Constants, KeystoreKeys}
import connectors.{AttachmentsConnector, FileUploadConnector, KeystoreConnector, S4LConnector}
import models.fileUpload.{Envelope, EnvelopeFile, Metadata}
import org.mockito.Matchers
import org.mockito.Mockito._
import org.scalatest.BeforeAndAfter
import org.scalatest.mock.MockitoSugar
import play.api.libs.json.Json
import play.api.libs.ws.WSResponse
import play.api.test.Helpers._
import uk.gov.hmrc.http.cache.client.CacheMap
import uk.gov.hmrc.play.test.{UnitSpec, WithFakeApplication}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }

class FileUploadServiceSpec extends UnitSpec with MockitoSugar with WithFakeApplication with BeforeAndAfter {

  val internalId = "Int-312e5e92-762e-423b-ac3d-8686af27fdb5"
  val mockFileUploadConnector = mock[FileUploadConnector]
  val mockS4LConnector = mock[S4LConnector]
  val mockKeystoreConnector = mock[KeystoreConnector]
  val mockAttachmentsConnector = mock[AttachmentsConnector]
  val envelopeID = "00000000-0000-0000-0000-000000000000"
  val fileID = 1
  val stringFileID = "1"
  val envelopeStatus = "OPEN"
  val fileNameValidPdf = "testFile.pdf"
  val fileNameValidDuplicatePdf = "test.pdf"
  val fileNameValidDuplicateJpeg = "test.jpeg"
  val fileNameValidDuplicateJpg = "test.jpg"
  val fileNameValidDuplicateXls = "test.xls"
  val fileNameValidDuplicateXlsx = "test.xlsx"
  val fileNameValidXls = "testFile.xls"
  val fileNameValidXlsx = "testFile.xlsx"
  val fileNameValidJpg = "testFile.jpg"
  val fileNameValidJpeg = "testFile.jpeg"
  val fileNameInvalidBmp = "testFile.bmp"
  val fileNameInvalidXml = "testFile.xml"
  val fileNameInvalidDocx = "testFile.docx"
  val fileNameInvalidDoc = "testFile.doc"
  implicit val hc = HeaderCarrier()
  implicit val user = TAVCUser(ggUser.allowedAuthContext, internalId)
  val tavcReferenceId = "XATAVC000123456"
  val oid = "00000001-0000-0000-0000-000000000000"


  val envelopeStatusResponse = Json.parse(s"""{
  |  "id": "$envelopeID",
  |  "callbackUrl": "test",
  |  "metadata": {
  |  },
  |  "status": "$envelopeStatus"
  |}""".stripMargin)

  val envelopeStatusWithFileResponse = Json.parse(s"""{
  |  "id": "$envelopeID",
  |  "callbackUrl": "test",
  |  "metadata": {
  |  },
  |  "files": [{
  |   "id": "1",
  |   "status": "PROCESSING",
  |   "name": "test.pdf",
  |   "contentType": "application/pdf",
  |   "length": 5242880,
  |   "created": "2016-03-31T12:33:45Z",
  |   "metadata": {
  |   },
  |   "href": "test.url"
  |  }],
  |  "status": "$envelopeStatus"
  |}""".stripMargin)

  val envelopeFullEnvelopAtLimit = Json.parse(s"""{
  |  "id": "$envelopeID",
  |  "callbackUrl": "test",
  |  "metadata": {
  |  },
  |  "files": [{
  |   "id": "1",
  |   "status": "PROCESSING",
  |   "name": "test.pdf",
  |   "contentType": "application/pdf",
  |   "length": 5242880,
  |   "created": "2016-03-31T12:33:45Z",
  |   "metadata": {
  |   },
  |   "href": "test.url"
  |  },
  |  {
  |   "id": "2",
  |   "status": "PROCESSING",
  |   "name": "test.xls",
  |   "contentType": "application/vnd.ms-excel",
  |   "length": 5242880,
  |   "created": "2016-03-31T12:33:45Z",
  |   "metadata": {
  |   },
  |   "href": "test.url"
  |  },{
  |   "id": "3",
  |   "status": "PROCESSING",
  |   "name": "test.xlsx",
  |   "contentType": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
  |   "length": 5242880,
  |   "created": "2016-03-31T12:33:45Z",
  |   "metadata": {
  |   },
  |   "href": "test.url"
  |  },{
  |   "id": "4",
  |   "status": "PROCESSING",
  |   "name": "test.jpg",
  |   "contentType": "image/jpeg",
  |   "length": 5242880,
  |   "created": "2016-03-31T12:33:45Z",
  |   "metadata": {
  |   },
  |   "href": "test.url"
  |  },{
  |   "id": "5",
  |   "status": "PROCESSING",
  |   "name": "test.jpeg",
  |   "contentType": "image/jpeg",
  |   "length": 5242880,
  |   "created": "2016-03-31T12:33:45Z",
  |   "metadata": {
  |   },
  |   "href": "test.url"
  |  }],
  |  "status": "$envelopeStatus"
  |}""".stripMargin)

  val envelopeFullEnvelopeMinus1Byte = Json.parse(s"""{
   |  "id": "$envelopeID",
   |  "callbackUrl": "test",
   |  "metadata": {
   |  },
   |  "files": [{
   |   "id": "1",
   |   "status": "PROCESSING",
   |   "name": "test.pdf",
   |   "contentType": "application/pdf",
   |   "length": 5242879,
   |   "created": "2016-03-31T12:33:45Z",
   |   "metadata": {
   |   },
   |   "href": "test.url"
   |  },
   |  {
   |   "id": "2",
   |   "status": "PROCESSING",
   |   "name": "test.xls",
   |   "contentType": "application/vnd.ms-excel",
   |   "length": 5242880,
   |   "created": "2016-03-31T12:33:45Z",
   |   "metadata": {
   |   },
   |   "href": "test.url"
   |  },{
   |   "id": "3",
   |   "status": "PROCESSING",
   |   "name": "test.xlsx",
   |   "contentType": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
   |   "length": 5242880,
   |   "created": "2016-03-31T12:33:45Z",
   |   "metadata": {
   |   },
   |   "href": "test.url"
   |  },{
   |   "id": "4",
   |   "status": "PROCESSING",
   |   "name": "test.jpg",
   |   "contentType": "image/jpeg",
   |   "length": 5242880,
   |   "created": "2016-03-31T12:33:45Z",
   |   "metadata": {
   |   },
   |   "href": "test.url"
   |  },{
   |   "id": "5",
   |   "status": "PROCESSING",
   |   "name": "test.jpeg",
   |   "contentType": "image/jpeg",
   |   "length": 5242880,
   |   "created": "2016-03-31T12:33:45Z",
   |   "metadata": {
   |   },
   |   "href": "test.url"
   |  }],
   |  "status": "$envelopeStatus"
   |}""".stripMargin)


  val createEnvelopeResponse = Json.parse(s"""{
  | "envelopeID": "$envelopeID"
  |}""".stripMargin)

  val files = Seq(EnvelopeFile("1","PROCESSING","test.pdf","application/pdf",Some(5242880),"2016-03-31T12:33:45Z",Metadata(None),"test.url"))

  val envelope = Envelope(envelopeID,envelopeStatus,None)
  val envelopeWithFiles = Envelope(envelopeID,envelopeStatus,
    Some(files))

  case class FakeWSResponse(status: Int) extends WSResponse {
    override def allHeaders = ???
    override def statusText = ???
    override def underlying[T] = ???
    override def xml = ???
    override def body = ???
    override def header(key: String) = ???
    override def cookie(name: String) = ???
    override def cookies = ???
    override def json = ???
    override def bodyAsBytes = ???
  }

  before{
    reset(mockFileUploadConnector)
  }

  object TestService extends FileUploadService {
    override lazy val fileUploadConnector = mockFileUploadConnector
    override lazy val s4lConnector = mockS4LConnector
    override lazy val attachmentsConnector = mockAttachmentsConnector
    override lazy val baseUrl = "https://www.my.pretenddomain.co.uk/"


  }

  "FileUploadService" should {

    "Use the correct FileUploadConnector" in {
      FileUploadService.fileUploadConnector shouldBe FileUploadConnector
    }

    "Use the correct S4LConnector" in {
      FileUploadService.s4lConnector shouldBe S4LConnector
    }

    "Use the correct SubmissionConnector" in {
      FileUploadService.attachmentsConnector shouldBe AttachmentsConnector
    }

  }


    "storeRedirectParameterIfValid" should {
      lazy val result = TestService.storeRedirectParameterIfValid("https://www.my.pretenddomain.co.uk/my-target-url", "test", mockKeystoreConnector)
      "return true if the parameter starts with the expected service base url from the configuration and does not start with //" in {
        when(mockKeystoreConnector.saveFormData(Matchers.any(), Matchers.any())(Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(CacheMap("", Map())))
        result shouldBe true
      }
    }

  "storeRedirectParameterIfValid" should {
    lazy val result = TestService.storeRedirectParameterIfValid("https://www.my.NOTMYpretenddomain.co.uk/my-target-url", "test", mockKeystoreConnector)
    "return false if the parameter does not start with the expected service base url from the configuration and does not start with //" in {
      when(mockKeystoreConnector.saveFormData(Matchers.any(), Matchers.any())(Matchers.any(), Matchers.any()))
        .thenReturn(Future.successful(CacheMap("", Map())))
      result shouldBe false
    }
  }

  "storeRedirectParameterIfValid" should {
    lazy val result = TestService.storeRedirectParameterIfValid("", "test", mockKeystoreConnector)
    "return true if the parameter is empty as nothign to validate" in {
      when(mockKeystoreConnector.saveFormData(Matchers.any(), Matchers.any())(Matchers.any(), Matchers.any()))
        .thenReturn(Future.successful(CacheMap("", Map())))
      result shouldBe true
    }
  }

    "storeRedirectParameterIfValid" should {
      "throw an exception if parameter starts with with //" in {
        intercept[IllegalArgumentException] {
          TestService.storeRedirectParameterIfValid("//www.my.pretenddomain.co.uk/my-target-url", "test", mockKeystoreConnector)
        }
      }
    }

    "validateFile" when {

    "the file name is unique, the file size is at limit and the file type is xls" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidXls, Constants.fileSizeLimit)

      "return Seq(true,true,true,true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe Seq(true, true, true, true)
      }

    }

    "the file name is unique, the file size is at limit and the file type is xlsx" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidXlsx, Constants.fileSizeLimit)

      "return Seq(true,true,true,true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe Seq(true, true, true, true)
      }

    }

    "the file name is unique, the file size is at limit and the file type is jpg" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidJpg, Constants.fileSizeLimit)

      "return Seq(true,true,true, true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe Seq(true, true, true, true)
      }

    }


    "the file name is unique, the file size is at limit and the file type is PDF" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidPdf, Constants.fileSizeLimit)

      "return Seq(true,true,true, true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe Seq(true, true, true, true)
      }

    }

    "the file name is unique, the file size is at limit and the file type is not an allowable type (txt)" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameInvalidBmp, Constants.fileSizeLimit)

      "return Seq(true,true,false,true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe Seq(true, true, false, true)
      }

    }

    "the file name is unique, the file size is at limit and the file type is not an allowable type (doc)" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameInvalidDoc, Constants.fileSizeLimit)

      "return Seq(true,true,false,true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe Seq(true, true, false, true)
      }

    }

    "the file name is unique, the file size is at limit and the file type is not an allowable type (docx)" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameInvalidDocx, Constants.fileSizeLimit)

      "return Seq(true,true,false,true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe Seq(true, true, false, true)
      }

    }

    "the file name is unique, the file size is at limit and the file type is an allowable type (pdf) but envelope already at 25MB limit" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidPdf, 1)

      "return Seq(true,true,true,false)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeFullEnvelopAtLimit))))
        await(result) shouldBe Seq(true, true, true, false)
      }

    }

    "the file name is unique, the file size is at limit and the file type is an allowable type (pdf) but envelope is 1 byte below limit" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidPdf, 1)

      "return Seq(true,true,true, true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeFullEnvelopeMinus1Byte))))
        await(result) shouldBe Seq(true, true, true, true)
      }

    }

    "the file name is unique, the file size is over at limit and the file type is not an allowable type" should {

      lazy val result = TestService.validateFile(envelopeID, fileNameInvalidXml, Constants.fileSizeLimit + 1)

      "return Seq(true,false,false, true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe Seq(true, false, false, true)
      }

    }

    "the file name is not unique, the file size is over the limit and the file type is PDF " should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidDuplicatePdf, Constants.fileSizeLimit + 1)

      "return Seq(false,false,true, true)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe Seq(false, false, true, true)
      }

    }

    "the file name is not unique, the file size is over the limit and the file type is XLS " should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidDuplicateXls, Constants.fileSizeLimit + 1)

      "return Seq(false,false,true, false)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeFullEnvelopAtLimit))))
        await(result) shouldBe Seq(false, false, true, false)
      }

    }

    "the file name is not unique, the file size is over the limit and the file type is XLSX " should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidDuplicateXlsx, Constants.fileSizeLimit + 1)

      "return Seq(false,false,true, false)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeFullEnvelopAtLimit))))
        await(result) shouldBe Seq(false, false, true, false)
      }

    }

    "the file name is not unique, the file size is over the limit and the file type is JPG " should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidDuplicateJpg, Constants.fileSizeLimit + 1)

      "return Seq(false,false,true, false)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeFullEnvelopAtLimit))))
        await(result) shouldBe Seq(false, false, true, false)
      }

    }

    "the file name is not unique, the file size is over the limit and the file type is JPEG " should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidDuplicateJpeg, Constants.fileSizeLimit + 1)

      "return Seq(false,false,true, false)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeFullEnvelopAtLimit))))
        await(result) shouldBe Seq(false, false, true, false)
      }

    }

    "the file name is not unique, the file size is over the limit, the file type is not an allowable type and exceeds envelope limit " should {

      lazy val result = TestService.validateFile(envelopeID, fileNameValidDuplicatePdf, Constants.fileSizeLimit + 1)

      "return Seq(false,false,false, false)" in {
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeFullEnvelopAtLimit))))
        await(result) shouldBe Seq(false, false, true, false)
      }

    }

  }

  "getEnvelopeID" when {

    "createNewID is true and envelopeID is in save4later" should {

      lazy val result = TestService.getEnvelopeID()

      "return an envelopeID if created successfully" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        await(result) shouldBe envelopeID
      }

    }

    "createNewID is true and envelopeID is not in save4later" should {

      lazy val result = TestService.getEnvelopeID()

      "return an envelopeID if created successfully" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(None))
        when(mockAttachmentsConnector.createEnvelope()(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(createEnvelopeResponse))))
        await(result) shouldBe envelopeID
      }

    }

    "createNewID is true and envelopeID is an empty string in save4later" should {

      lazy val result = TestService.getEnvelopeID()

      "return an envelopeID if created successfully" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some("")))
        when(mockAttachmentsConnector.createEnvelope()(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(createEnvelopeResponse))))
        await(result) shouldBe envelopeID
      }

    }

    "createNewID is false and envelopeID is not in save4later" should {

      lazy val result = TestService.getEnvelopeID(createNewID = false)

      "return an envelopeID if created successfully" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(None))
        await(result) shouldBe ""
      }

    }

  }

  "checkEnvelopeStatus" when {

    "getEnvelopeID returns a non empty string and getEnvelopeStatus returns OK" should {

      lazy val result = TestService.checkEnvelopeStatus(envelopeID)

      "Return the envelope" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusResponse))))
        await(result) shouldBe Some(envelope)
      }

    }

    "getEnvelopeID returns a non empty string and getEnvelopeStatus returns non OK" should {

      lazy val result = TestService.checkEnvelopeStatus(envelopeID)

      "Return None" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(INTERNAL_SERVER_ERROR)))
        await(result) shouldBe None
      }

    }

  }

  "uploadFile" when {

    val testFile = ByteString("1")

    "The envelope has no files and the file is uploaded successfully" should {

      lazy val result = TestService.uploadFile(testFile, fileNameValidPdf, envelopeID)

      "Return OK" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusResponse))))
        when(mockFileUploadConnector.addFileContent(Matchers.eq(envelopeID), Matchers.eq(1), Matchers.eq(fileNameValidPdf),
          Matchers.eq(testFile), Matchers.eq(TestService.PDF))(Matchers.any())).thenReturn(Future.successful(FakeWSResponse(OK)))
        await(result).status shouldBe OK
      }

    }

    "The envelope has a file and the file is uploaded successfully" should {

      lazy val result = TestService.uploadFile(testFile, fileNameValidPdf, envelopeID)

      "Return OK" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        when(mockFileUploadConnector.addFileContent(Matchers.eq(envelopeID), Matchers.eq(2), Matchers.eq(fileNameValidPdf),
          Matchers.eq(testFile), Matchers.eq(TestService.PDF))(Matchers.any())).thenReturn(Future.successful(FakeWSResponse(OK)))
        await(result).status shouldBe OK
      }

    }

    "The file is not uploaded successfully" should {

      lazy val result = TestService.uploadFile(testFile, fileNameValidPdf, envelopeID)

      "Return OK" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        when(mockFileUploadConnector.addFileContent(Matchers.eq(envelopeID), Matchers.eq(2), Matchers.eq(fileNameValidPdf),
          Matchers.eq(testFile), Matchers.eq(TestService.PDF))(Matchers.any())).thenReturn(Future.successful(FakeWSResponse(INTERNAL_SERVER_ERROR)))
        await(result).status shouldBe INTERNAL_SERVER_ERROR
      }

    }

  }

  "getEnvelopeFiles" when {

    "checkEnvelopeStatus returns an envelope with a file" should {

      lazy val result = TestService.getEnvelopeFiles(envelopeID)

      "return a sequence with envelope files in it" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusWithFileResponse))))
        await(result) shouldBe files
      }

    }

    "checkEnvelopeStatus returns an envelope with no files" should {

      lazy val result = TestService.getEnvelopeFiles(envelopeID)

      "return an empty Seq" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockAttachmentsConnector.getEnvelopeStatus(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK,Some(envelopeStatusResponse))))
        await(result) shouldBe Seq()
      }

    }

  }

  "closeEnvelope" when {

    "getEnvelopeID returns a non-empty envelope ID, addMetadataFile returns OK and closeEnvelope returns non OK" should {

      lazy val result = TestService.closeEnvelope(tavcReferenceId, envelopeID, oid)

      "return the http response" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockFileUploadConnector.addFileContent(Matchers.eq(envelopeID), Matchers.any(), Matchers.eq(s"$envelopeID-metatdata.xml"), Matchers.any(),
          Matchers.eq(TestService.XML))(Matchers.any())).thenReturn(Future.successful(FakeWSResponse(OK)))
        when(mockAttachmentsConnector.closeEnvelope(Matchers.eq(envelopeID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(INTERNAL_SERVER_ERROR)))
        await(result).status shouldBe INTERNAL_SERVER_ERROR
      }

    }

    "getEnvelopeID returns a non-empty envelope ID, addMetadataFile returns non OK" should {

      lazy val result = TestService.closeEnvelope(tavcReferenceId, envelopeID, oid)

      "return the http response" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockFileUploadConnector.addFileContent(Matchers.eq(envelopeID), Matchers.any(), Matchers.eq(s"$envelopeID-metadata.xml"), Matchers.any(),
          Matchers.eq(TestService.XML))(Matchers.any())).thenReturn(Future.successful(FakeWSResponse(INTERNAL_SERVER_ERROR)))
        await(result).status shouldBe INTERNAL_SERVER_ERROR
      }

    }

  }

  "deleteFile" when {

    "getEnvelopeID returns a non-empty envelope ID, deleteFile returns OK" should {

      lazy val result = TestService.deleteFile(stringFileID)

      "return the http response" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockAttachmentsConnector.deleteFile(Matchers.eq(envelopeID), Matchers.eq(stringFileID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(OK)))
        await(result).status shouldBe OK
      }

    }

    "getEnvelopeID returns a non-empty envelope ID, deleteFile returns non-OK" should {

      lazy val result = TestService.deleteFile(stringFileID)

      "return the http response" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some(envelopeID)))
        when(mockAttachmentsConnector.deleteFile(Matchers.eq(envelopeID), Matchers.eq(stringFileID))(Matchers.any()))
          .thenReturn(Future.successful(HttpResponse(INTERNAL_SERVER_ERROR)))
        await(result).status shouldBe INTERNAL_SERVER_ERROR
      }

    }

    "getEnvelopeID returns an empty envelope ID" should {

      lazy val result = TestService.deleteFile(stringFileID)

      "return INTERNAL_SERVER_ERROR" in {
        when(mockS4LConnector.fetchAndGetFormData[String](Matchers.eq(KeystoreKeys.envelopeID))(Matchers.any(), Matchers.any(), Matchers.any()))
          .thenReturn(Future.successful(Some("")))
        await(result).status shouldBe INTERNAL_SERVER_ERROR
      }

    }
  }

}
