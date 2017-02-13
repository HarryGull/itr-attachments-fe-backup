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

package controllers

import akka.util.ByteString
import config.FrontendGlobal.{badRequestTemplate, internalServerErrorTemplate}
import auth.AuthorisedAndEnrolledForTAVC
import common.KeystoreKeys
import config.{FrontendAppConfig, FrontendAuthConnector}
import connectors.{EnrolmentConnector, KeystoreConnector}
import play.Logger
import play.api.data.FormError
import play.api.i18n.Messages
import play.api.mvc.{Action, AnyContent, MultipartFormData, Result}
import services.FileUploadService
import uk.gov.hmrc.play.frontend.controller.FrontendController
import utils.{MultipartFormDataParser, Transformers}
import views.html.fileUpload.FileUpload
import uk.gov.hmrc.play.http.HeaderCarrier
import play.api.i18n.Messages.Implicits._
import play.api.Play.current

import scala.concurrent.Future

object FileUploadController extends FileUploadController {
  override lazy val applicationConfig = FrontendAppConfig
  override lazy val authConnector = FrontendAuthConnector
  override lazy val enrolmentConnector = EnrolmentConnector
  override lazy val fileUploadService = FileUploadService
  val keyStoreConnector: KeystoreConnector = KeystoreConnector
}

trait FileUploadController extends FrontendController with AuthorisedAndEnrolledForTAVC {

  val fileUploadService: FileUploadService
  val keyStoreConnector: KeystoreConnector

  def show(continueUrl: Option[String], backUrl: Option[String]): Action[AnyContent] = AuthorisedAndEnrolled.async {
    implicit user => implicit request =>

      val urlBack = backUrl.fold("")(_.toString)
      val urlContinue = continueUrl.fold("")(_.toString)

      def processQueryString(cUrl:String, bUrl:String) = {

        if (cUrl.length > 0) {
          keyStoreConnector.saveFormData(KeystoreKeys.continueUrl, cUrl)
        }

        if (bUrl.length > 0) {
          keyStoreConnector.saveFormData(KeystoreKeys.backUrl, bUrl)
        }
      }

      processQueryString(urlContinue, urlBack)

      for {
        envelopeID <- fileUploadService.getEnvelopeID()
        files <- fileUploadService.getEnvelopeFiles(envelopeID)
        savedUrl <- keyStoreConnector.fetchAndGetFormData[String](KeystoreKeys.continueUrl)
        savedBackUrl <- keyStoreConnector.fetchAndGetFormData[String](KeystoreKeys.backUrl)
      } yield (envelopeID, files, savedUrl, savedBackUrl) match {
        case (_, _, None, _) if continueUrl.fold("")(_.toString).length == 0 => {
          Logger.warn(s"[FileUploadController][show] - Required Continue Url not passed")
          BadRequest(badRequestTemplate)
        }
        case (_, _, _, None) if backUrl.fold("")(_.toString).length == 0 => {
          //InternalServerError("Required back Url not passsed")
          Logger.warn(s"[FileUploadController][show] - Required back Url not passed")
          BadRequest(badRequestTemplate)
        }
        case (_, _, _, _) if envelopeID.nonEmpty => {
          Ok(FileUpload(files, envelopeID, if (urlBack.length > 0) urlBack else savedBackUrl.getOrElse("")))
        }
        case (_, _, _, _) => InternalServerError(internalServerErrorTemplate)
      }
  }

  val submit = AuthorisedAndEnrolled.async { implicit user => implicit request =>
    def routeRequest(url: Option[String], envelopeId: String): Future[Result] = {
      url match {
        case Some(data) if data.length > 0 => {
          keyStoreConnector.clearKeystore()
          Future.successful(Redirect(s"$data?envelopeId=$envelopeId"))
        }
        case _ => {
          keyStoreConnector.clearKeystore()
          Future.successful(InternalServerError(internalServerErrorTemplate))
        }
      }
    }

    for {
      continueUrl <- keyStoreConnector.fetchAndGetFormData[String](KeystoreKeys.continueUrl)
      envelopeId <- FileUploadService.getEnvelopeID(false)
      route <- routeRequest(continueUrl, envelopeId)

    } yield route
  }

  def upload: Action[MultipartFormData[ByteString]] = Action.async(parse.multipartFormData[ByteString]
    (MultipartFormDataParser.handleFilePartAsFile)) {

    implicit request =>
      def processErrors(envelopeID: String, errors: Seq[Boolean])(implicit hc: HeaderCarrier): Future[Result] = {
        for {
          files <- fileUploadService.getEnvelopeFiles(envelopeID)
          url <- keyStoreConnector.fetchAndGetFormData[String](KeystoreKeys.backUrl)
        } yield (files, url) match {
          case (_, _) => BadRequest(FileUpload(files, envelopeID, url.get, generateFormErrors(errors)))
        }
      }

      val envelopeID = request.body.dataParts("envelope-id").head
      fileUploadService.belowFileNumberLimit(envelopeID).flatMap {
        case true =>
          if (request.body.file("supporting-docs").isDefined) {
            val file = request.body.file("supporting-docs").get
            fileUploadService.validateFile(envelopeID, file.filename, file.ref.length).flatMap {
              case Seq(true, true, true) =>
                fileUploadService.uploadFile(file.ref, file.filename, envelopeID).map {
                  case response if response.status == OK => Redirect(routes.FileUploadController.show())
                  case _ => InternalServerError(internalServerErrorTemplate)
                }
              case errors =>
                processErrors(envelopeID, errors)
            }
          }
          else Future.successful(Redirect(routes.FileUploadController.show()))
        case false => Future.successful(Redirect(routes.FileUploadController.show()))
      }
  }

//  def closeEnvelope(tavcRef: String, envelopeId: String, id: String): Action[AnyContent] = Action.async { implicit request =>
//    fileUploadService.closeEnvelope(tavcRef, envelopeId, id).map {
//      responseReceived =>
//        Status(responseReceived.status)(responseReceived.body)
//    }
//  }

  private def generateFormErrors(errors: Seq[Boolean]): Seq[FormError] = {
    val messages = Seq(
      "duplicate-name" -> Messages("page.fileUpload.limit.name"),
      "over-size-limit" -> Messages("page.fileUpload.limit.size"),
      "invalid-format" -> Messages("page.fileUpload.limit.type")
    )
    def createSequence(index: Int = 0, output: Seq[(String, String)] = Seq()): Seq[(String, String)] = {
      if (!errors(index))
        if (index < errors.length - 1) createSequence(index + 1, output :+ messages(index))
        else output :+ messages(index)
      else if (index < errors.length - 1) createSequence(index + 1, output)
      else output
    }
    Transformers.errorBuilder(createSequence())
  }


}
