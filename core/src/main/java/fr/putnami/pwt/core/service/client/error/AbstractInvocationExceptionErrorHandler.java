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
package fr.putnami.pwt.core.service.client.error;

import fr.putnami.pwt.core.service.shared.exception.CommandException;
import fr.putnami.pwt.core.service.shared.service.CommandService;

public abstract class AbstractInvocationExceptionErrorHandler
	extends AbstractCommandExceptionErrorHandler {

	@Override
	protected boolean internalHandle(CommandException error) {
		if (CommandService.EXCEPTION_INVOKATION.equals(error.getMessage())) {
			return this.handleCommandException(error);
		}
		return false;
	}

	protected abstract boolean handleCommandException(CommandException error);
}
