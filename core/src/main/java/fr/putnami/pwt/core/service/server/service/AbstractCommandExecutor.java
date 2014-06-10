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
package fr.putnami.pwt.core.service.server.service;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import fr.putnami.pwt.core.service.shared.domain.CommandDefinition;
import fr.putnami.pwt.core.service.shared.exception.CommandException;

public abstract class AbstractCommandExecutor implements CommandExecutor {

	protected final Log logger = LogFactory.getLog(CommandExecutor.class);

	private CommandDefinition commandDefinition;

	public void setCommandDefinition(CommandDefinition commandDefinition) {
		this.commandDefinition = commandDefinition;
	}

	@Override
	public CommandDefinition getCommandDefinition() {
		return this.commandDefinition;
	}

	protected Throwable toThrown(String message, Throwable source) {
		CommandException thrown = new CommandException(message, source);
		if (source != null) {
			StringBuffer buf = new StringBuffer();
			for (StackTraceElement element : source.getStackTrace()) {
				buf.append(element.toString()).append("\n");
			}
			thrown.setCauseStackTrace(buf.toString());
		}
		return thrown;
	}

	protected Object toReturnPayload(Object result) {
		return result;
	}

}
