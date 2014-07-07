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

import java.util.List;

import com.google.gwt.activity.shared.Activity;
import com.google.gwt.event.shared.EventBus;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.IsWidget;

import fr.putnami.pwt.core.mvp.client.event.MayStopActivityEvent;
import fr.putnami.pwt.core.mvp.client.event.StartActivityEvent;
import fr.putnami.pwt.core.mvp.client.event.StopActivityEvent;
import fr.putnami.pwt.core.service.client.CallbackAdapter;
import fr.putnami.pwt.core.service.client.CommandController;
import fr.putnami.pwt.core.service.shared.domain.CommandResponse;

public final class MvpActivity implements Activity, ViewProxy.Callback {

	private final MvpPlace place;
	private AcceptsOneWidget panel;
	private IsWidget view;

	public MvpActivity(MvpPlace place) {
		super();
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
		ViewProxy proxy = this.place.getViewProxy();
		if (proxy != null) {
			proxy.getView(this);
		}
	}

	@Override
	public void showView(IsWidget view) {
		this.startView(this.panel, view);
	}

	protected void startView(final AcceptsOneWidget container, final IsWidget widget) {
		CommandController commandController = CommandController.get();
		boolean isSuspended = commandController.isSuspended();
		commandController.setSuspended(true);
		StartActivityEvent.fire(this, this.place, container, widget);
		if (widget instanceof View) {
			((View) widget).present(this.place);
		}
		commandController.flush(new CallbackAdapter<List<CommandResponse>>() {

			@Override
			public void onSuccess(List<CommandResponse> result) {
				container.setWidget(widget);
			};
		});
		commandController.setSuspended(isSuspended);
	}

}
