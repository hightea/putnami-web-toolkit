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
package fr.putnami.pwt.core.service.client.event;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HasHandlers;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.service.shared.domain.CommandRequest;

public final class CommandRequestEvent extends GwtEvent<CommandRequestEvent.Handler> {

	public interface Handler extends EventHandler {

		void onCommandRequest(CommandRequestEvent event);
	}

	public interface HasCommandRequestHandlers extends HasHandlers {

		HandlerRegistration addCommandRequestHandler(Handler handler);
	}

	public static final Type<Handler> TYPE = new Type<Handler>();

	private final long requestId;
	private final CommandRequest command;

	public CommandRequestEvent(long requestId, CommandRequest command) {
		super();
		this.requestId = requestId;
		this.command = command;
	}

	public long getRequestId() {
		return requestId;
	}

	public CommandRequest getCommand() {
		return command;
	}

	@Override
	protected void dispatch(Handler handler) {
		handler.onCommandRequest(this);
	}

	@Override
	public Type<Handler> getAssociatedType() {
		return CommandRequestEvent.TYPE;
	}

}
