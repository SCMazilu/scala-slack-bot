package io.scalac.slack.bots.system

import io.scalac.slack.bots.IncomingMessageListener
import io.scalac.slack.{MessageEventBus, SlackBot}
import io.scalac.slack.common.{BaseMessage, Command}

/**
 * Created on 13.02.15 23:36
 */
class CommandsRecognizerBot extends IncomingMessageListener {

  val commandChar = '$'

  override def receive: Receive = {

    case bm @ BaseMessage(text, channel, user, dateTime, edited) =>
      //COMMAND links list with bot's nam jack can be called:
      // jack link list
      // jack: link list
      // @jack link list
      // @jack: link list
      // $link list
      def changeIntoCommand(pattern: String): Boolean = {
        if (text.trim.startsWith(pattern)) {
          val tokenized = text.trim.drop(pattern.length).trim.split("\\s")
          publish(Command(tokenized.head, tokenized.tail.toList, bm))
          true
        }
        false
      }

      //call by commad character
      if (!changeIntoCommand(commandChar.toString))
        SlackBot.botInfo match {
          case Some(bi) =>
            //call by name
            changeIntoCommand(bi.name + ":") ||
              changeIntoCommand(bi.name) ||
              //call by ID
              changeIntoCommand(s"<@${bi.id}>:") ||
              changeIntoCommand(s"<@${bi.id}>")

          case None => //nothing to do!

        }

  }

  override def bus: MessageEventBus = SlackBot.eventBus
}