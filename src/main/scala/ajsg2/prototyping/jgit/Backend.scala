package ajsg2.prototyping.jgit

import java.io.{File, IOException}
import java.net.{MalformedURLException, URI, URISyntaxException, URL}

import ajsg2.prototyping.jgit.exceptions._
import org.eclipse.jgit.api.{CloneCommand, Git}

/**
  * Created by Adam on 26/11/2016.
  */
object Backend {

  var git : Git = _
  var workingDir : File = _

  def main(args: Array[String]): Unit = {
    /*val builder = new FileRepositoryBuilder();
    val repository = builder.setGitDir(new File("test repo"))
      .readEnvironment() // scan environment GIT_* variables
      .findGitDir() // scan up the file system tree
      .build();

    git = new Git(repository)*/

    try{
      setDirectory("D:\\Libraries\\OneDrive\\Documents\\Project\\prototyping\\backend\\testingfolder\\jgit-cookbook")
      clone("https://github.com/centic9/jgit-cookbook.git")
      git.close()
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
        println("Clone operation completed successfully")
      }
    } catch {
      case
        e: MalformedURLException  => throw new IOException("Clone failed: Malformed URL")
        e: URISyntaxException  => throw new IOException("Clone failed: Malformed URL")
    }

  }
}
