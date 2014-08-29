package fr.putnami.pwt.tutorial.client.welcome;

import com.google.gwt.core.client.GWT;

import fr.putnami.pwt.core.mvp.client.MvpPlace;
import fr.putnami.pwt.core.mvp.client.ViewProxy;

public class WelcomePlace extends MvpPlace {

	public static final WelcomePlace INSTANCE = new WelcomePlace();

	public WelcomePlace() {
		super((ViewProxy) GWT.create(WelcomeView.class), null);
	}

	@Override
	public MvpPlace getPlace(String token) {
		return WelcomePlace.INSTANCE;
	}
}
