import mill._
import mill.scalalib._

def scalaVer = "2.13.1"

object patterns extends ScalaModule {
  def scalaVersion = scalaVer
  def ivyDeps = Agg(
    ivy"${scalaOrganization()}:scala-reflect:${scalaVersion()}"
  )
}
