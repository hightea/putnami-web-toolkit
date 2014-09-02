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

import com.google.gwt.activity.shared.Activity;
import com.google.gwt.event.shared.EventBus;
import com.google.gwt.place.shared.Place;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.mvp.client.event.MayStopActivityEvent;
import fr.putnami.pwt.core.mvp.client.event.StartActivityEvent;
import fr.putnami.pwt.core.mvp.client.event.StopActivityEvent;

public final class ViewActivity implements Activity, ViewProxy.Callback {

	private final Place place;
	private final ViewProxy viewMapper;
	private AcceptsOneWidget panel;
	private IsWidget view;

	public ViewActivity(ViewProxy viewMapper, Place place) {
		super();
		this.viewMapper = viewMapper;
		this.place = place;
	}

	@Override
	public String mayStop() {
		return MayStopActivityEvent.fire(this, this.place, view).getMessage();
	}

	@Override
	public void onCancel() {
		StopActivityEvent.fire(this, this.place, view, true);
	}

	@Override
	public void onStop() {
		StopActivityEvent.fire(this, this.place, view, false);
	}

	@Override
	public void start(AcceptsOneWidget panel, EventBus eventBus) {
		this.panel = panel;
		this.viewMapper.getView(this);
	}

	@Override
	public void showView(IsWidget view) {
		StartActivityEvent.fire(this, this.place, this.panel, view);
		if (view instanceof Presenter) {
			((Presenter) view).present(this.place, this.panel);
		}
	}

}
