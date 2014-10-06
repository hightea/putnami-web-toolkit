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
package fr.putnami.pwt.plugin.code.client.event;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HandlerRegistration;
import com.google.gwt.event.shared.HasHandlers;

public class LiveValueChangeEvent extends GwtEvent<LiveValueChangeEvent.Handler> {

	public interface Handler extends EventHandler {

		void onLiveValueChange(LiveValueChangeEvent event);
	}

	public interface HasLiveValueChangeHandlers extends HasHandlers {

		HandlerRegistration addLiveValueChangeHandler(Handler handler);
	}

	public static final Type<Handler> TYPE = new Type<Handler>();

	public static void fireIfNotEqual(HasLiveValueChangeHandlers source, String oldValue,
			String newValue) {
		if (LiveValueChangeEvent.shouldFire(oldValue, newValue)) {
			LiveValueChangeEvent event = new LiveValueChangeEvent(newValue);
			source.fireEvent(event);
		}
	}

	protected static boolean shouldFire(String oldValue, String newValue) {
		return oldValue != newValue && (oldValue == null || !oldValue.equals(newValue));
	}

	private final String value;

	protected LiveValueChangeEvent(String value) {
		super();
		this.value = value;
	}

	@Override
	public GwtEvent.Type<Handler> getAssociatedType() {
		return LiveValueChangeEvent.TYPE;
	}

	@Override
	protected void dispatch(Handler handler) {
		handler.onLiveValueChange(this);
	}

	public String getValue() {
		return this.value;
	}
}
