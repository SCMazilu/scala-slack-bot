package io.scalac.slack.bots

import io.scalac.slack.common._

/**
 * Created on 16.02.15 23:11
 */
class ImportantMessageBot extends IncomingMessageListener {

  log.debug(s"Starting $this")

  def setImportant(message: BaseMessage, params: List[String]) = {
    publish(RichOutboundMessage(message.channel, List(
      Attachment(Color.danger, Title("Attention please!!"), PreText("<!group>"), Text(params.mkString(" ")))
    )))
  }

  override def receive: Receive = {
    case Command("important", params, m) => setImportant(m, params)
    case Command("!", params, m) => setImportant(m, params)
    case bm: BaseMessage if bm.text.matches("""((^|\s*)(!{2,3})\s+.*)""") =>
       setImportant(bm, bm.text.trim.split(' ').toList.tail)

  }
}