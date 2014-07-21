/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt.  If not, see <http://www.gnu.org/licenses/>.
 */
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
