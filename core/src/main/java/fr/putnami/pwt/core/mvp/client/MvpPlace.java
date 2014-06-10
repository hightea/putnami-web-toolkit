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
package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceTokenizer;

public abstract class MvpPlace extends Place implements PlaceTokenizer<MvpPlace> {

	private final ViewProxy viewProxy;

	private MvpPlace parent;
	private final String token;

	public MvpPlace(MvpPlace place) {
		this(place.viewProxy, place.token);
	}

	public MvpPlace(MvpPlace place, String token) {
		this(place.viewProxy, token);
	}

	public MvpPlace(ViewProxy viewProxy, String token) {
		this.viewProxy = viewProxy;
		this.token = token;
	}

	public ViewProxy getViewProxy() {
		return this.viewProxy;
	}

	@Override
	public String getToken(MvpPlace place) {
		return place == null ? null : place.token;
	}

	public MvpPlace getParent() {
		return this.parent;
	}

	public void setParent(MvpPlace parent) {
		this.parent = parent;
	}

	public String getToken() {
		return this.token;
	}

}
