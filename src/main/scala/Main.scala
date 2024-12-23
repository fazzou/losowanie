import courier._
import Defaults._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.Random

object Main {

  case class Person(name: String, mail: String)

  case class Config(
      smtpHost: String,
      smtpPort: Int,
      user: String,
      pass: String,
      people: Seq[Person],
      forbiddenPairs: Set[(Person, Person)],
      messageTitle: String,
      messageTemplate: String
  )

  private def readConfig(path: String) = {
    val source = Source.fromFile(path)
    val lines = source.getLines().toList
    source.close()

    lines match {
      case smtp :: user :: pass :: rest =>
        val smtpResult = smtp.split(":") match {
          case Array(smtpHost, smtpPortString) =>
            smtpPortString.toIntOption
              .toRight("cannot parse smtp port")
              .map(smtpHost -> _)
          case _ => Left("cannot parse smtp host and port")
        }
        val peopleLines = rest.tail.takeWhile(_.nonEmpty)
        val (peopleErrors, peopleList) = peopleLines
          .map(_.split(" "))
          .partitionMap {
            case Array(name, mail) => Right(name -> Person(name, mail))
            case arr => Left(s"cannot parse ${arr.mkString} as person")
          }
        val people = peopleList.toMap
        val forbiddenPairsLines =
          rest.drop(peopleLines.length + 2).takeWhile(_.nonEmpty)
        val (pairsErrors, forbiddenPairs) = forbiddenPairsLines
          .map(_.split(" "))
          .partitionMap {
            case Array(one, two) =>
              (people.get(one) zip people.get(two))
                .toRight(s"people not defined for $one or $two")
            case _ => Left("invalid format of forbidden pairs")
          }
        val errors = peopleErrors ++ pairsErrors

        val maybeMessageTitle = rest
          .drop(peopleLines.length + forbiddenPairsLines.length + 3)
          .headOption
        val messageTemplate = rest
          .drop(peopleLines.length + forbiddenPairsLines.length + 4)
          .mkString("\n")

        for {
          smtpPair <- smtpResult
          (smtpHost, smtpPort) = smtpPair
          messageTitle <- maybeMessageTitle.toRight("no message title found")
          config = Config(
            smtpHost,
            smtpPort,
            user,
            pass,
            people.values.toSeq,
            forbiddenPairs.toSet,
            messageTitle,
            messageTemplate
          )
          result <- Either.cond(errors.isEmpty, config, errors.mkString("\n"))
        } yield result
      case _ => Left("not enough information in configuration")
    }
  }

  def main(args: Array[String]): Unit = {
    args.headOption
      .toRight("expected one argument")
      .flatMap { path =>
        readConfig(path)
      }
      .fold(println, randomizeAndSend)
  }

  private def randomizeAndSend(config: Config): Unit = {
    val mailer = Mailer(config.smtpHost, config.smtpPort)
      .auth(true)
      .as(config.user, config.pass)
      .ssl(true)
      .startTls(true)()

    val envelopes =
      randomize(config.people.toList, config.forbiddenPairs.toSeq).map {
        case (a, b) =>
          envelope(config, a, b)
      }

    val future = Future.traverse(envelopes) { envelope =>
      Thread.sleep(2000)
      mailer.apply(envelope).map { _ =>
        println("Sent mail...")
      }
    }
    Await.result(future, Duration.Inf)
  }

  private def randomize(
      people: List[Person],
      forbiddenPairs: Seq[(Person, Person)]
  ): List[(Person, Person)] = {
    var randomized = List.empty[(Person, Person)]
    do {
      val shuffled = Random.shuffle(people)
      randomized = shuffled.zip(shuffled.last :: shuffled)
    } while (randomized.exists(forbiddenPairs.contains))
    randomized
  }

  private def envelope(
      config: Config,
      mailReceiver: Person,
      giftReceiver: Person
  ): Envelope = {
    Envelope
      .from(config.user.addr)
      .to(mailReceiver.mail.addr)
      .subject(config.messageTitle)
      .content(
        Text(text(config.messageTemplate, mailReceiver.name, giftReceiver.name))
      )
  }

  private def text(
      template: String,
      messageReceiver: String,
      giftReceiver: String
  ) = {
    template
      .replaceAll("\\$messageReceiver", messageReceiver)
      .replaceAll("\\$giftReceiver", giftReceiver)
  }
}
