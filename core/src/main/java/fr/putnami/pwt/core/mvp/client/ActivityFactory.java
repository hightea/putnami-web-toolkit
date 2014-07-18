package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.activity.shared.Activity;
import com.google.gwt.place.shared.Place;

public interface ActivityFactory {

	String[] getPlacePrefixes();

	Activity createActivity(Place place);
}
