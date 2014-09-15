package fr.putnami.pwt.tutorial.client;

import fr.putnami.pwt.core.inject.client.Module;
import fr.putnami.pwt.core.inject.client.annotation.MvpDescription;
import fr.putnami.pwt.core.widget.client.OneWidgetPanel;

import fr.putnami.pwt.tutorial.client.welcome.WelcomePlace;

@MvpDescription(
		display = OneWidgetPanel.class,
		defaultPlace = WelcomePlace.class,
		activities = {
				WelcomePlace.class
		})
public class TutorialApp implements Module {

}
