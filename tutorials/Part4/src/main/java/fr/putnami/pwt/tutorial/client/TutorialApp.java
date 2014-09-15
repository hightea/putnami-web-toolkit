package fr.putnami.pwt.tutorial.client;

import fr.putnami.pwt.core.inject.client.Module;
import fr.putnami.pwt.core.inject.client.annotation.MvpDescription;

import fr.putnami.pwt.tutorial.client.about.AboutPlace;
import fr.putnami.pwt.tutorial.client.application.PageDecorator;
import fr.putnami.pwt.tutorial.client.contact.ContactPlace;
import fr.putnami.pwt.tutorial.client.welcome.WelcomePlace;

@MvpDescription(
		display = PageDecorator.class,
		defaultPlace = WelcomePlace.class,
		activities = {
				WelcomePlace.class,
				AboutPlace.class,
				ContactPlace.class
		})
public class TutorialApp implements Module {

}
