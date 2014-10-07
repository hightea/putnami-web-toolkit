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

import com.google.gwt.thirdparty.guava.common.collect.Lists;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.List;

import fr.putnami.pwt.core.service.shared.domain.CommandDefinition;
import fr.putnami.pwt.core.service.shared.domain.CommandRequest;
import fr.putnami.pwt.core.service.shared.domain.CommandResponse;
import fr.putnami.pwt.core.service.shared.service.CommandService;

public class CommandExecutorImpl extends AbstractCommandExecutor {
	protected final Log executorLogger;

	private final Object service;
	private final Method method;

	public CommandExecutorImpl(Object service, Method method) {
		this.service = service;
		this.method = method;

		String[] paramTypes = new String[method.getParameterTypes().length];

		for (int i = 0; i < method.getParameterTypes().length; i++) {
			paramTypes[i] = method.getParameterTypes()[i].getName();
		}
		CommandDefinition definition =
			new CommandDefinition(method.getDeclaringClass().getName(), method.getName(), method
				.getReturnType().getName(),
				paramTypes);
		this.setCommandDefinition(definition);

		this.executorLogger = LogFactory.getLog(method.getDeclaringClass().getCanonicalName());
	}

	@Override
	public CommandResponse executeCommand(CommandRequest request) {
		List<?> argsArray = request.getArgs();
		CommandResponse reponse = new CommandResponse();
		Throwable thrown = null;
		reponse.setRequestId(request.getRequestId());
		try {
			this.executorLogger.info("execute : " + this.getCommandDefinition());
			Object result = this.method.invoke(this.service, argsArray.toArray());
			reponse.setResult(Lists.newArrayList(result));
		} catch (IllegalArgumentException e) {
			thrown = e;
			reponse.setThrown(this.toThrown(CommandService.EXCEPTION_ILLEGAL_ARGUMENT, e));
		} catch (IllegalAccessException e) {
			thrown = e;
			reponse.setThrown(this.toThrown(CommandService.EXCEPTION_ILLEGAL_ACCESS, e));
		} catch (InvocationTargetException e) {
			thrown = e.getCause();
			reponse.setThrown(this.toThrown(CommandService.EXCEPTION_INVOKATION, e.getTargetException()));
		}

		if (thrown != null) {
			this.executorLogger.error("Invokation failled ", thrown);
		}

		return reponse;
	}

}
