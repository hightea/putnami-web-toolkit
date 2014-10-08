/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.core.mvp.client;

import com.google.gwt.activity.shared.Activity;
import com.google.gwt.event.shared.EventBus;
import com.google.gwt.place.shared.Place;
import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.user.client.ui.AcceptsOneWidget;

import fr.putnami.pwt.core.mvp.client.util.MvpUtils;

public abstract class ActionPlace extends Place implements Activity, ActivityFactory, PlaceTokenizer<ActionPlace> {

	private final String prefix;

	protected ActionPlace() {
		this.prefix = MvpUtils.getDefaultPrefix(this);
	}

	public ActionPlace(String prefix) {
		this.prefix = prefix;
	}

	@Override
	public String[] getPlacePrefixes() {
		return new String[] {this.prefix};
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
		// NoOp
	}

	@Override
	public void onStop() {
		// NoOp
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
		this.run();
	}

	public abstract void run();

}
