package com.kittens.Actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.kittens.Models.Kitten

object KittenCollector {
  def props(threshold: Int): Props = Props(new KittenCollector(threshold))

  final case class ReceiveKitten(kitten: Kitten)
  final case class RegisterVeterinarian(vet: ActorRef)
  final case class VaccinateKittens(kittens: Seq[Kitten])
}

class KittenCollector(threshold: Int) extends Actor with ActorLogging {
  import KittenCollector._

  var veterinarian: Option[ActorRef] = None

  var unvaccinatedKittens: Seq[Kitten] = Seq.empty

  override def receive: Receive = {
    case RegisterVeterinarian(vet) =>
      log.info("Local vet registered!")

      veterinarian = Some(vet)

      if(unvaccinatedKittens.length >= threshold) {
        log.info("Unvaccinated kitty threshold reached! Sending to vet...")
        vet ! VaccinateKittens(unvaccinatedKittens)
        unvaccinatedKittens = Seq.empty
      }

    case ReceiveKitten(kitten) =>
      log.info("Kitten collected! Vaccinated = {}", kitten.vaccinated)
      if(!kitten.vaccinated) {
        unvaccinatedKittens = kitten +: unvaccinatedKittens
      }

      if(unvaccinatedKittens.length >= threshold && veterinarian.isDefined) {
        log.info("Unvaccinated kitty threshold reached! Sending to vet...")
        veterinarian.get ! VaccinateKittens(unvaccinatedKittens)
        unvaccinatedKittens = Seq.empty
      }
  }
}