package ajsg2.prototyping.jgit

import akka.actor.{Actor, ActorContext}
import spray.routing._
import spray.json._
import DefaultJsonProtocol._
//import spray.http._
//import MediaTypes._
//import akka.actor.Actor.Receive

/**
  * Created by Adam on 25/01/2017.
  */
// we don't implement our route structure directly in the service actor because
// we want to be able to test it independently, without having to spin up an actor
class MyServiceActor extends Actor with MyService {

    // the HttpService trait defines only one abstract member, which
    // connects the services environment to the enclosing actor or test
    def actorRefFactory: ActorContext = context

    // this actor only runs our route, but you could add
    // other things here, like request stream processing
    // or timeout handling
    def receive: Receive = runRoute(myRoute)
}


// this trait defines our service behavior independently from the service actor
trait MyService extends HttpService {

    val myRoute: Route =
        path("graph") {
            Backend.setDirectory("C:\\Users\\Adam\\OneDrive\\Documents\\Project\\prototyping\\backend\\testingfolder\\test")
            //Backend.clone("https://github.com/csete/gqrx.git")
            Backend.loadRepository()
            println("Loaded repo")
            complete {

                Backend.buildCommitGraph()
                println("Sending graph JSON")
                Backend.generateJson()
            }
        } ~
        path("changes") {
            complete(Backend.detectChangedFiles().toJson.compactPrint)
        } ~
        path("commit") {
            entity(as[String]) { json =>
                val files = json.parseJson.convertTo[List[(String, String)]]
                if(Backend.commit(files))
                    complete("Committed sucessfully")
                else
                    complete("No changes to commit")
            }
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
