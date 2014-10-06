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
package fr.putnami.pwt.core.service.client.event;

import com.google.gwt.event.shared.EventHandler;
import com.google.gwt.event.shared.GwtEvent;
import com.google.gwt.event.shared.HasHandlers;
import com.google.web.bindery.event.shared.HandlerRegistration;

import fr.putnami.pwt.core.service.shared.domain.CommandRequest;
import fr.putnami.pwt.core.service.shared.domain.CommandResponse;

public final class CommandResponseEvent extends GwtEvent<CommandResponseEvent.Handler> {

	public interface Handler extends EventHandler {

		void onCommandResponse(CommandResponseEvent event);
	}

	public interface HasCommandResponseHandlers extends HasHandlers {

		HandlerRegistration addCommandResponseHandler(Handler handler);
	}

	public static final Type<Handler> TYPE = new Type<Handler>();

	private final long requestId;
	private final CommandRequest command;
	private final CommandResponse response;

	public CommandResponseEvent(long requestId, CommandRequest command, CommandResponse response) {
		super();
		this.requestId = requestId;
		this.command = command;
		this.response = response;
	}

	public long getRequestId() {
		return this.requestId;
	}

	public CommandRequest getCommand() {
		return this.command;
	}

	public CommandResponse getResponse() {
		return this.response;
	}

	@Override
	protected void dispatch(Handler handler) {
		handler.onCommandResponse(this);
	}

	@Override
	public Type<Handler> getAssociatedType() {
		return CommandResponseEvent.TYPE;
	}

}
