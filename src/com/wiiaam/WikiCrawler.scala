package com.wiiaam

import java.io._
import java.net.{HttpURLConnection, URL}
import java.security.SecureRandom
import java.security.cert.X509Certificate
import java.util
import java.util.regex.Pattern
import javax.net.ssl._

object WikiCrawler {
  val crawlQueue: util.ArrayDeque[(String, String)] = new util.ArrayDeque[(String, String)]()
  var target = ""
  def main(args: Array[String]) {


    if(args.length < 2){
      println("Please specify a link to start on and a link to end with")
    }
    else{
      setAllowAllCerts()
      crawlQueue.add((args(0), args(0)))
      target = args(1)
      while(true){
        try {
          val next = crawlQueue.poll()
          if(next == null) {
            println("Ran out of links")
            return
          }
          crawl(next._1, next._2)
        } catch {
          case e: Exception =>
        }
      }
    }

  }

  def crawl(url: String, path: String): Unit ={
    println(s"Crawling: $url found at path $path")
    val page = readUrl(url)
    val m = Pattern.compile("href=\"\\/wiki\\/.*<\\/a>").matcher(page)
    while(m.find()){
      val next = m.group()
      val link = "https://en.wikipedia.org/wiki/" + next.split("wiki/")(1).split("\"")(0)
      val aName = next.split(">")(1).split("</a")(0)
      if(!next.contains("File:")) {
        crawlQueue.add((link, path + " -> " + link.split(".*\\/")(1).replace("_"," ") + s" (under $aName)"))
      }
      if(target == link){
        println("Target found")
        println("Path: " + path + " -> " + link.split(".*\\/")(1).replace("_"," ")  + s" (under $aName)")
        System.exit(0)
      }
    }
  }
  def readUrl(urlString: String): String = {
    val url = new URL(urlString)
    if (urlString.startsWith("https")) {
      val urlc = url.openConnection().asInstanceOf[HttpsURLConnection]
      urlc.setInstanceFollowRedirects(true)
      urlc.addRequestProperty("Accept-Language", "en-US,en;q=0.8")
      urlc.addRequestProperty("User-Agent", "WikiCrawler")
      urlc.connect()
      val reader = new BufferedReader(new InputStreamReader(urlc.getInputStream))
      val buffer = new StringBuffer()
      val chars = new Array[Char](1024)
      var reading = true
      while (reading) {
        val read = reader.read(chars)
        if (read != -1) buffer.append(chars, 0, read)
        else reading = false
      }

      buffer.toString
    }
    else {
      val urlc = url.openConnection().asInstanceOf[HttpURLConnection]
      urlc.setInstanceFollowRedirects(true)
      urlc.addRequestProperty("Accept-Language", "en-US,en;q=0.8")
      urlc.addRequestProperty("User-Agent", "Mozilla")
      urlc.connect()
      val reader = new BufferedReader(new InputStreamReader(urlc.getInputStream))
      val buffer = new StringBuffer()
      val chars = new Array[Char](1024)
      var reading = true
      while (reading) {
        val read = reader.read(chars)
        if (read != -1) buffer.append(chars, 0, read)
        else reading = false
      }

      buffer.toString
    }
  }

  def setAllowAllCerts(): Unit ={
    val tm = new X509TrustManager {
      override def getAcceptedIssuers: Array[X509Certificate] = null

      override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}

      override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}
    }
    val tmarray: Array[TrustManager] = Array(tm)
    val context = SSLContext.getInstance("SSL")
    context.init(new Array[KeyManager](0), tmarray, new SecureRandom())
    val sslfact: SSLSocketFactory = context.getSocketFactory
    HttpsURLConnection.setDefaultSSLSocketFactory(sslfact)
  }
}
