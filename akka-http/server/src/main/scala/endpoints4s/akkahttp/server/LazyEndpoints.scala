package endpoints4s.akkahttp.server

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.{Directive1, Directives, Route}
import endpoints4s.{Tupler, Valid, Validated}
import endpoints4s.algebra.Documentation

trait LazyEndpoints extends Endpoints with endpoints4s.algebra.LazyEndpoints {

  override def request[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, Out](
    m: Method,
    u: Url[UrlP],
    e: RequestEntity[BodyP] = emptyRequest,
    d: Documentation = None,
    h: RequestHeaders[HeadersP] = emptyRequestHeaders,
  )(implicit
    tuplerUB: Tupler.Aux[UrlP, BodyP, UrlAndBodyPTupled],
    tuplerUBH: Tupler.Aux[UrlAndBodyPTupled, HeadersP, Out],
  ): LazyRequest[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, Out] =
    new RequestPayload[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, Out] with Request[Out] { self =>
      override def method: Method = m
      override def url: Url[UrlP] = u
      override def entity: RequestEntity[BodyP] = e
      override def docs: Documentation = d
      override def headers: RequestHeaders[HeadersP] = h

      // This is unfortunately a copy of the current implementation in Endpoints since we cannot access it from here as
      // far as I am aware
      override val directive: Directive1[Out] = {
        val methodDirective = convToDirective1(Directives.method(method))
        val headersDirective: Directive1[HeadersP] =
          directive1InvFunctor.xmapPartial[Validated[HeadersP], HeadersP](
            Directives.extractRequest.map(headers.decode),
            identity,
            c => Valid(c)
          )
        val matchDirective = methodDirective & url.directive & headersDirective
        matchDirective.tflatMap { case (_, a, c) =>
          entity.map(b => tuplerUBH(tuplerUB(a, b), c))
        }
      }
      // idem
      override def uri(out: Out): Uri = {
        val (ab, _) = tuplerUBH.unapply(out)
        val (a, _) = tuplerUB.unapply(ab)
        url.uri(a)
      }
    }
  
  override def endpoint[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In, Out](
    r: LazyRequest[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In],
    resp: Response[Out],
    d: EndpointDocs,
  )(implicit
    tuplerUB: Tupler.Aux[UrlP, BodyP, UrlAndBodyPTupled],
    tuplerUBH: Tupler.Aux[UrlAndBodyPTupled, HeadersP, In],
  ): LazyEndpoint[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In, Out] =
    new EndpointPayload[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In, Out] with Endpoint[In, Out] {
      override def docs: EndpointDocs = d
      override val request: LazyRequest[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In] = r
      override val response: Out => Route = resp
    }

}
