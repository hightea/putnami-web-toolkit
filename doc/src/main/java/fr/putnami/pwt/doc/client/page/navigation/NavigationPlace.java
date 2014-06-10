/**
 * This file is part of pwt-doc.
 *
 * pwt-doc is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * pwt-doc is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with pwt-doc.  If not, see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.page.navigation;

import com.google.gwt.core.client.GWT;

import fr.putnami.pwt.core.mvp.client.ViewProxy;

public class NavigationPlace extends fr.putnami.pwt.core.mvp.client.MvpPlace {

	public static final NavigationPlace INSTANCE = new NavigationPlace();

	public NavigationPlace() {
		super((ViewProxy) GWT.create(NavigationView.class), null);
	}

	@Override
	public fr.putnami.pwt.core.mvp.client.MvpPlace getPlace(String token) {
		return NavigationPlace.INSTANCE;
	}
}
