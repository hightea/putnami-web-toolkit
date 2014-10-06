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
package fr.putnami.pwt.core.service.client;

import com.google.gwt.core.client.GWT;
import com.google.gwt.user.client.rpc.AsyncCallback;

import java.util.List;

import fr.putnami.pwt.core.service.client.event.CommandRequestEvent.HasCommandRequestHandlers;
import fr.putnami.pwt.core.service.client.event.CommandResponseEvent.HasCommandResponseHandlers;
import fr.putnami.pwt.core.service.shared.domain.CommandDefinition;
import fr.putnami.pwt.core.service.shared.domain.CommandRequest;
import fr.putnami.pwt.core.service.shared.domain.CommandResponse;

public abstract class CommandController implements
		HasCommandRequestHandlers,
		HasCommandResponseHandlers {

	private static CommandController instance;

	public static CommandController get() {
		if (instance == null) {
			instance = GWT.create(CommandController.class);
		}
		return instance;
	}

	public abstract CommandRequest invokeCommand(CommandDefinition commandDefinition, CommandParam commandParam);

	public abstract int flush();

	public abstract int flush(AsyncCallback<List<CommandResponse>> callback);

	public abstract int countPendingRequest();

	public abstract boolean isSuspended();

	public abstract void setSuspended(boolean suspended);

}
