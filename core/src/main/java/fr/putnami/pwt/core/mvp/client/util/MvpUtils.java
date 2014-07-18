package fr.putnami.pwt.core.mvp.client.util;

import com.google.gwt.place.shared.Place;

import fr.putnami.pwt.core.mvp.client.ActivityFactory;

public final class MvpUtils {

	public static String getPlacePrefix(Place place) {
		if (place instanceof ActivityFactory) {
			String[] prefixes = ((ActivityFactory) place).getPlacePrefixes();
			if (prefixes != null && prefixes.length > 0) {
				String prefix = prefixes[0];
				if (!prefix.startsWith("!")) {
					prefix = "!" + prefix;
				}
				return prefix;
			}
		}
		return getDefaultPrefix(place);
	}

	public static String getDefaultPrefix(Place place) {
		return "!" + place.getClass().getSimpleName().replaceAll("Place$", "");
	}

	private MvpUtils(){}
}
