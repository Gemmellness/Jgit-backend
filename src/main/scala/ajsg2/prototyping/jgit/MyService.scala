package ajsg2.prototyping.jgit

import akka.actor.Actor
import spray.routing._
import spray.http._
import MediaTypes._

/**
  * Created by Adam on 25/01/2017.
*/
// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class MyServiceActor extends Actor with MyService {

    // the HttpService trait defines only one abstract member, which
    // connects the services environment to the enclosing actor or test
    def actorRefFactory = context

    // this actor only runs our route, but you could add
    // other things here, like request stream processing
    // or timeout handling
    def receive = runRoute(myRoute)
}


// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService {

    val myRoute =
        path("graph") {
            Backend.setDirectory("C:\\Users\\Adam\\Documents\\GitHub\\Project\\Jgit-backend\\testingfolder\\solarhud")
            //Backend.clone("https://github.com/Stochast1c/solarhud.git")
            Backend.loadRepository()
            Backend.buildCommitGraph()
            println("Sending graph JSON")
            complete(Backend.generateJson())
        } ~
        path("") {
            compressResponse() {
                getFromResource("index.html")
            }
        } ~
        {
            getFromResourceDirectory("")
        }
}
