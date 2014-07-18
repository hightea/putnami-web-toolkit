package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.activity.shared.Activity;
import com.google.gwt.event.shared.EventBus;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.user.client.ui.AcceptsOneWidget;

import fr.putnami.pwt.core.mvp.client.util.MvpUtils;

public class ActionPlace extends Place implements Activity, ActivityFactory, PlaceTokenizer<ActionPlace>, Runnable {

	private final String prefix;

	protected ActionPlace(){
		this.prefix = MvpUtils.getDefaultPrefix(this);
	}
	public ActionPlace(String prefix) {
		this.prefix = prefix;
	}

	@Override
	public String[] getPlacePrefixes() {
		return new String[] {
				prefix
		};
	}

	@Override
	public Activity createActivity(Place place) {
		return this;
	}


	@Override
	public String mayStop() {
		return null;
	}

	@Override
	public void onCancel() {
	}

	@Override
	public void onStop() {
	}

	@Override
	public ActionPlace getPlace(String token) {
		return this;
	}

	@Override
	public String getToken(ActionPlace place) {
		return null;
	}

	@Override
	public void start(AcceptsOneWidget panel, EventBus eventBus) {
		run();
	}

	@Override
	public void run() {

	}

}
