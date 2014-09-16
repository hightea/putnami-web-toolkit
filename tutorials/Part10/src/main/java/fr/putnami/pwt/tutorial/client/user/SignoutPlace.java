package fr.putnami.pwt.tutorial.client.user;

import fr.putnami.pwt.core.mvp.client.ActionPlace;
import fr.putnami.pwt.core.mvp.client.MvpController;
import fr.putnami.pwt.core.security.client.controller.SessionController;

public class SignoutPlace extends ActionPlace {

	@Override
	public void run() {
		SessionController.get().signOut();
		MvpController.get().goToDefaultPlace();
	}
}
