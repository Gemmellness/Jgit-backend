package ajsg2.prototyping.jgit

import java.io.{File, IOException}
import java.lang.Iterable
import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.util.Date

import ajsg2.prototyping.jgit.exceptions._
import org.eclipse.jgit.api.{CloneCommand, Git}
import org.eclipse.jgit.lib.Repository
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.storage.file.FileRepositoryBuilder

import scala.collection.JavaConverters._
import scala.collection.mutable

import scalax.collection.Graph
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
import scalax.collection.edge.Implicits._

/**
  * Created by Adam on 26/11/2016.
  */
object Backend {

  var git : Git = _
  var repository : Repository = _
  var workingDir : File = _

  def main(args: Array[String]): Unit = {
    try{
      setDirectory("D:\\Libraries\\OneDrive\\Documents\\Project\\prototyping\\backend\\testingfolder\\jgit-cookbook")
      loadRepository()
      buildCommitGraph()
      //clone("https://github.com/centic9/jgit-cookbook.git")

    }catch {
      case e: Exception => System.err.println("Exception handled:")
        e.printStackTrace()
    }
  }

  /**
    * Set the working directory to the one specified in the argument.
    *
    * @param address The (absolute) address of the directory
    */
  @throws[WorkingDirectoryAddressException]
  def setDirectory(address: String): Unit = {
    val dir: File = new File(address)

    if(!dir.isAbsolute)
      throw WorkingDirectoryAddressException("Working directory change failed: address is not absolute")
    else
      workingDir = dir
  }

   /**
    * Builds the commit graph, built of Commits and labelled directed edges
    */
  def buildCommitGraph(): Unit = {
    val nodes = new mutable.HashMap[String, Commit]
    val edges = new mutable.ListBuffer[DiEdge[Commit]]

    // Generate nodes
    val commits: Iterable[RevCommit] = git.log().all().call()

    commits.asScala.foreach(commit => {
      // Build commit object
      val date : Date = new Date(commit.getCommitTime.toLong*1000)
      val c = Commit(commit.getName, commit.getAuthorIdent.getName + ", " + commit.getAuthorIdent.getEmailAddress, "", date.toString, 0)

      nodes += ((c.hash, c))
    })

    // Generate edges
    val commits2 = git.log().all().call()

    commits2.asScala.foreach(commit => {
      val parents = commit.getParents
      val default = Commit("error", "error", new Date().toString, "", 0)

      parents.foreach( (p : RevCommit) => edges += nodes.getOrElse(p.getName, default) ~> nodes.getOrElse(commit.getName, default))

    })

    val nodesArray : Array[Commit] = nodes.toArray[Commit]
    val edgesArray: Array[DiEdge[Commit]] = edges.toArray

    Graph.from(nodesArray, edgesArray)
  }

  /**
    * Load the repository in the current working directory
    */
  def loadRepository(): Unit = {
    val builder = new FileRepositoryBuilder()
    val repo = builder.setGitDir(new File(workingDir.toString  + "\\.git"))
      .readEnvironment() // scan environment GIT_* variables
      .findGitDir() // scan up the file system tree
      .build()

    repository = repo
    git = new Git(repo)
  }

  /**
    * @param url The URL of the repository to clone
    */
  @throws[IOException]
  @throws[CloneDirectoryExistsException]
  def clone(url: String): Unit = {
    try {
      val uri: URI = new URL(url).toURI

      val clone: CloneCommand = Git.cloneRepository
        .setURI(uri.toString)
        .setDirectory(workingDir)

      if (workingDir.exists)
        throw CloneDirectoryExistsException("The directory " + workingDir.toString + " already exists")
      else {
        git = clone.call()
        repository = git.getRepository
        git.close()
        println("Clone operation completed successfully")
      }
    } catch {
      case
        _: MalformedURLException  => throw new IOException("Clone failed: Malformed URL")
        _: URISyntaxException  => throw new IOException("Clone failed: Malformed URL")
    }

  }

  case class Commit(hash:String, author:String, branch:String, date:String, depth:Int) {

  }
}


