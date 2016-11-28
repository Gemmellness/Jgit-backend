package ajsg2.prototyping.jgit

import java.io.{File, IOException}
import java.net.{MalformedURLException, URI, URISyntaxException, URL}
import java.util.Date

import ajsg2.prototyping.jgit.exceptions._
import org.eclipse.jgit.api.{CloneCommand, Git}
import org.eclipse.jgit.revwalk.RevCommit
import org.eclipse.jgit.storage.file.FileRepositoryBuilder

import scalax.collection.Graph
import scalax.collection.edge.LDiEdge

/**
  * Created by Adam on 26/11/2016.
  */
object Backend {

  var git : Git = _
  var repository = _
  var workingDir : File = _

  def main(args: Array[String]): Unit = {
    try{
      setDirectory("D:\\Libraries\\OneDrive\\Documents\\Project\\prototyping\\backend\\testingfolder\\jgit-cookbook")
      loadRepository()
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

  /*
   * Builds the commit graph, built of Commits and labelled directed edges
   */
  def buildCommitGraph(): Graph[Commit, LDiEdge] = {
    val nodes = Nil
    val edges = Nil

    Graph.from(nodes, edges)
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

  class Commit {
    val hash = ""
    val author = ""
    val branch = ""
    val date = new Date(0L)
  }
}


