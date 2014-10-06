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
package fr.putnami.pwt.core.service.server.service;

import com.google.common.collect.Maps;

import java.lang.reflect.Method;
import java.util.Map;

import fr.putnami.pwt.core.service.shared.domain.CommandDefinition;
import fr.putnami.pwt.core.service.shared.exception.CommandException;

public class CommandExecutorRegistryImpl implements CommandExecutorRegistry {

	private final Map<CommandDefinition, CommandExecutor> executors = Maps.newHashMap();

	public CommandExecutorRegistryImpl() {
	}

	@Override
	public CommandExecutor resolveCommandExecutor(CommandDefinition commandDef) {
		CommandExecutor executor = this.executors.get(commandDef);
		if (executor == null) {
			throw new CommandException("no executor foud for " + commandDef);
		}
		return executor;
	}

	@Override
	public void injectService(Class<?> clazz, Object service) {
		for (Method method : clazz.getMethods()) {
			CommandExecutorImpl executor = new CommandExecutorImpl(service, method);
			this.executors.put(executor.getCommandDefinition(), executor);
		}
	}

}
