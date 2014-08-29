package fr.putnami.pwt.tutorial.client.about;

import com.google.gwt.core.client.GWT;

import fr.putnami.pwt.core.mvp.client.MvpPlace;
import fr.putnami.pwt.core.mvp.client.ViewProxy;

public class AboutPlace extends MvpPlace {

	public static final AboutPlace INSTANCE = new AboutPlace();

	public AboutPlace() {
		super((ViewProxy) GWT.create(AboutView.class), null);
	}

	@Override
	public MvpPlace getPlace(String token) {
		return AboutPlace.INSTANCE;
	}
}
