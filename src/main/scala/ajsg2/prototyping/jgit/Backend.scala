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
import scalax.collection.edge.LDiEdge

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
    val dir: File = new File(address + "\\.git")

    if(!dir.isAbsolute)
      throw WorkingDirectoryAddressException("Working directory change failed: address is not absolute")
    else
      workingDir = dir
  }

  /**
    * Load the repository in the current working directory
    */
  def loadRepository(): Unit = {
    val builder = new FileRepositoryBuilder()
    val repo = builder.setGitDir(workingDir)
      .readEnvironment() // scan environment GIT_* variables
      .findGitDir() // scan up the file system tree
      .build()

    repository = repo
    git = new Git(repo)
  }

  /**
    * Builds the commit graph, built of Commits and labelled directed edges
    */
  def buildCommitGraph(): Unit = {
    val nodes = new mutable.HashMap[String, Commit]
    val edges = Nil

    // Iterate over all commits
    val commits: Iterable[RevCommit] = git.log().all().call()

    commits.asScala.foreach(commit => {

      // Build commit object
      val date : Date = new Date(commit.getCommitTime.toLong*1000)
      val c = new Commit(commit.getName, commit.getAuthorIdent.getName + ", " + commit.getAuthorIdent.getEmailAddress, "master", date.toString)

      nodes += ((c.hash, c))
    })

    val nodesArray : Array[Commit] = new Array(nodes.size)
    nodes.copyToArray[Commit](nodesArray)

    Graph.from(nodesArray, edges)
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
        e: MalformedURLException  => throw new IOException("Clone failed: Malformed URL")
        e: URISyntaxException  => throw new IOException("Clone failed: Malformed URL")
    }

  }

  class Commit(hashCode:String, authorName:String, branchName:String, dateCommited:String) {
    val hash: String = hashCode
    val author: String = authorName
    val branch: String = branchName
    val date: String = dateCommited
  }
}


