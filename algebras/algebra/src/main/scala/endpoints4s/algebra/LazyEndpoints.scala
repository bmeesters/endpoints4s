package endpoints4s.algebra

import endpoints4s.{Tupler, algebra}

/**
  * This shows an example of how the LazyEndpoints can be created and interop with the non-lazy variants, so it becomes
  * opt-in. Using overloading it has the same API, but a more precise type.
  */
trait ExampleWithLazy extends LazyEndpoints with algebra.JsonSchemas with JsonEntitiesFromSchemas {

  // Dummy domain and JsonSchema
  case class Location(id: Int)
  implicit val locationJsonSchema: Record[Location] =
    field[Int]("id", None).xmap(Location.apply)(location => location.id)

  // Type parameters are still hard to read, but URL=Unit, body=Location, headers=Unit, headsWithBody=Locatiuon, output=Location
  val exampleLazyRequest: LazyRequest[Unit, Location, Unit, Location, Location] =
    request(Put, path / "location", jsonRequest[Location])

  // A LazyRequest is a subtype of Request, so this works without any trouble
  val exampleAsOldRequest: Request[Location] = exampleLazyRequest

  // An example of an endpoint, again the type parameters are hard to read since it are so many. There might be some improvements possible
  val examplePut: LazyEndpoint[Unit, Location, Unit, Location, Location, String] =
    endpoint(
      exampleLazyRequest,
      ok(jsonResponse[String]),
      EndpointDocs()
    )

  // Aagain a LazyEndpoint == Endpoint so we can just use it as before
  val examplePutAsOldEndpoint: Endpoint[Location, String] = examplePut

  // Just here to show the composability
  def withError[A](original: Response[A]): Response[Either[String, A]] =
    response(BadRequest, jsonResponse[String]).orElse(original)

  // Since we can now access the payload we can create a new endpoint without providing the same information again
  // This is currently not possible without LazyEndpoint
  def composedEndpoint: LazyEndpoint[Unit, Location, Unit, Location, Location, Either[String, String]] =
    endpoint(examplePut.request, withError(examplePut.response), examplePut.docs)

}

// Implementation of LazyEndpoint and LazyRequest, as a PoC LazyResponse is not done
trait LazyEndpoints extends Endpoints {

  // A LazyRequest has a payload and is a normal request for interopability
  type LazyRequest[U, B, H, UB, A] = Request[A] with RequestPayload[U, B, H, UB, A]
  type LazyEndpoint[U, B, H, UB, In, Out] = Endpoint[In, Out] with EndpointPayload[U, B, H, UB, In, Out]

  // Payload is just all fields. Needs to be a trait for easy mix-in
  trait EndpointPayload[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In, Out] {
    def request: LazyRequest[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In]
    def response: Response[Out]
    def docs: EndpointDocs
  }

  trait RequestPayload[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, A] {
    def method: Method
    def url: Url[UrlP]
    def entity: RequestEntity[BodyP]
    def docs: Documentation
    def headers: RequestHeaders[HeadersP]
  }

  // Uses Scala's ability for overloading to provide the same API for a user perspective.
  def request[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, Out](
    method: Method,
    url: Url[UrlP],
    entity: RequestEntity[BodyP] = emptyRequest,
    docs: Documentation = None,
    headers: RequestHeaders[HeadersP] = emptyRequestHeaders,
  )(implicit
    tuplerUB: Tupler.Aux[UrlP, BodyP, UrlAndBodyPTupled],
    tuplerUBH: Tupler.Aux[UrlAndBodyPTupled, HeadersP, Out],
  ): LazyRequest[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, Out]

  def endpoint[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In, Out](
    request: LazyRequest[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In],
    response: Response[Out],
    docs: EndpointDocs,
  )(implicit
    tuplerUB: Tupler.Aux[UrlP, BodyP, UrlAndBodyPTupled],
    tuplerUBH: Tupler.Aux[UrlAndBodyPTupled, HeadersP, In],
  ): LazyEndpoint[UrlP, BodyP, HeadersP, UrlAndBodyPTupled, In, Out]

}
