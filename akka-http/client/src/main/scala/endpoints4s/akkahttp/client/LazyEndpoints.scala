package endpoints4s.akkahttp.client

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.stream.Materializer
import endpoints4s.algebra.Documentation
import endpoints4s.{Tupler, algebra}

import scala.collection.immutable.Seq
import scala.concurrent.{ExecutionContext, Future}

object ExampleClient  {
  implicit val as: ActorSystem = ActorSystem()
  implicit val mat: Materializer = Materializer.matFromSystem
  implicit val ec: ExecutionContext = as.dispatcher

  val client = new LazyEndpoints(EndpointsSettings(AkkaHttpRequestExecutor.cachedHostConnectionPool("", 1234)))
  with algebra.ExampleLazyEndpoints
  with endpoints4s.ujson.JsonSchemas
  with JsonEntitiesFromSchemas
  import client._

  val requestPayload: LazyRequest[Unit, Location, Unit, Location, Location] =
    composedEndpoint.request

  // Still as before, you can just use apply
  val result: Future[Either[String, String]] = composedEndpoint(Location(1))
}

abstract class LazyEndpoints(
  settings: EndpointsSettings
)(implicit ec: ExecutionContext, mat: Materializer)
    extends Endpoints(settings)
    with algebra.LazyEndpoints { outer =>

  override def endpoint[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In, Out](
    req: LazyRequest[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In],
    resp: Response[Out],
    doc: EndpointDocs,
  )(implicit
    tuplerUB: Tupler.Aux[UrlP, BodyP, UrlAndBodyPTupled],
    tuplerUBH: Tupler.Aux[UrlAndBodyPTupled, HeadersP, In],
  ): LazyEndpoint[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In, Out] = {
    // Copy-pasted logic from the non-lazy client
    new EndpointPayload[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In, Out] with Endpoint[In, Out] { self =>
      override def apply(a: In): Future[Out] =
        request(a).flatMap { httpResponse =>
          decodeResponse(response, httpResponse) match {
            case Some(entityB) =>
              entityB(httpResponse.entity).flatMap(futureFromEither)
            case None =>
              httpResponse.entity
                .discardBytes() // See https://github.com/akka/akka-http/issues/1495
              Future.failed(
                new Throwable(
                  s"Unexpected response status: ${httpResponse.status.intValue()}"
                )
              )
          }
        }
      override def request: LazyRequest[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In] = req
      override def response: (StatusCode, Seq[HttpHeader]) => Option[HttpEntity => Future[Either[Throwable, Out]]] = resp
      override def docs: EndpointDocs = doc
    }
  }

  override def request[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, Out](
    m: HttpRequest => HttpRequest,
    u: Url[UrlP],
    e: (BodyP, HttpRequest) => HttpRequest,
    d: Documentation,
    h: (HeadersP, List[HttpHeader]) => List[HttpHeader],
  )(implicit
    tuplerUB: Tupler.Aux[UrlP, BodyP, UrlAndBodyPTupled],
    tuplerUBH: Tupler.Aux[UrlAndBodyPTupled, HeadersP, Out],
  ): LazyRequest[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, Out] =
    new RequestPayload[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, Out] with Request[Out] { self =>
      override def method: HttpRequest => HttpRequest = m
      override def url: Url[UrlP] = u
      override def entity: (BodyP, HttpRequest) => HttpRequest = e
      override def docs: Documentation = d
      override def headers: (HeadersP, List[HttpHeader]) => List[HttpHeader] = h

      // idem copy-pasted
      override def apply(abc: Out): Future[HttpResponse] = {
        val (ab, c) = tuplerUBH.unapply(abc)
        val (a, b) = tuplerUB.unapply(ab)
        val uri =
          if (settings.baseUri == Uri("/")) Uri(url.encode(a))
          else Uri(s"${settings.baseUri.path}${url.encode(a)}")

        val request = method(entity(b, HttpRequest(uri = uri)))
          .withHeaders(headers(c, List.empty))

        settings.requestExecutor(request)
      }
    }

}
