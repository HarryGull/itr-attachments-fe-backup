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

package connectors

import config.TavcSessionCache
import play.api.libs.json.Format
import uk.gov.hmrc.http.cache.client.{CacheMap, SessionCache}

import scala.concurrent.Future
import uk.gov.hmrc.http.{ HeaderCarrier, HttpResponse }
import uk.gov.hmrc.play.http.logging.MdcLoggingExecutionContext._

object KeystoreConnector extends KeystoreConnector

trait KeystoreConnector {

  val sessionCache : SessionCache = TavcSessionCache

  def saveFormData[T](key: String, data : T)(implicit hc: HeaderCarrier, format: Format[T]): Future[CacheMap] = {
    sessionCache.cache[T](key, data)
  }

  def fetchAndGetFormData[T](key : String)(implicit hc: HeaderCarrier, format: Format[T]): Future[Option[T]] = {
    sessionCache.fetchAndGetEntry(key)
  }

  def clearKeystore()(implicit hc : HeaderCarrier) : Future[HttpResponse] = {
    sessionCache.remove()
  }

  def fetch()(implicit hc : HeaderCarrier) : Future[Option[CacheMap]] = {
    sessionCache.fetch()
  }
}
