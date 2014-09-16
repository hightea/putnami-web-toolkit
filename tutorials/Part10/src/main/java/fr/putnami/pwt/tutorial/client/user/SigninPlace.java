package fr.putnami.pwt.tutorial.client.user;

import com.google.gwt.place.shared.Place;

import fr.putnami.pwt.core.mvp.client.ViewPlace;
import fr.putnami.pwt.core.mvp.client.annotation.ActivityDescription;

@ActivityDescription(view = SigninPage.class)
public class SigninPlace extends ViewPlace {

	private Place fallback;

	public SigninPlace() {
	}

	public SigninPlace(Place fallback) {
		this.fallback = fallback;
	}

	public Place getFallback() {
		return fallback;
	}

}