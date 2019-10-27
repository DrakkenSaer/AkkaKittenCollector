package com.kittens

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import com.kittens.Actors.KittenCollector
import com.kittens.Actors.KittenCollector.{ReceiveKitten, RegisterVeterinarian, VaccinateKittens}
import com.kittens.Models.Kitten
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class KittenCollectorSpec
  extends TestKit(ActorSystem("KittenCollectorSpec"))
    with ImplicitSender
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A KittenCollector" must {
    "send its kittens to the vet when it has reached its threshold" in {

      val veterinarian = TestProbe()
      val kittenCollector = system.actorOf(KittenCollector.props(10))
      kittenCollector ! RegisterVeterinarian(veterinarian.ref)

      val kittens: IndexedSeq[Kitten] = (1 to 10) map { _ => Kitten(false) }
      kittens foreach { kitten =>  kittenCollector ! ReceiveKitten(kitten) }

      veterinarian.expectMsg(VaccinateKittens(kittens))
    }

    "send its should only count unvaccinated kittens" in {

      val veterinarian = TestProbe()
      val kittenCollector = system.actorOf(KittenCollector.props(10))
      kittenCollector ! RegisterVeterinarian(veterinarian.ref)

      val kittens: IndexedSeq[Kitten] = (1 to 20) map { n => Kitten(n % 2 === 0) }
      kittens foreach { kitten =>  kittenCollector ! ReceiveKitten(kitten) }

      veterinarian.expectMsg(VaccinateKittens(kittens.filter(!_.vaccinated)))
    }
  }

}