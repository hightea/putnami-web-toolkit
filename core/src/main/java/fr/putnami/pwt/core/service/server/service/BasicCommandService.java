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

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.gwt.user.server.rpc.AbstractRemoteServiceServlet;
import com.google.gwt.user.server.rpc.RPC;
import com.google.gwt.user.server.rpc.RPCRequest;
import com.google.gwt.user.server.rpc.RPCServletUtils;
import com.google.gwt.user.server.rpc.SerializationPolicy;
import com.google.gwt.user.server.rpc.SerializationPolicyProvider;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.util.List;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import fr.putnami.pwt.core.service.shared.domain.CommandRequest;
import fr.putnami.pwt.core.service.shared.domain.CommandResponse;
import fr.putnami.pwt.core.service.shared.service.CommandService;

public class BasicCommandService extends AbstractRemoteServiceServlet
	implements CommandService, SerializationPolicyProvider {

	private static final String PARAM_SERVICES = "services";

	private final Log logger = LogFactory.getLog(this.getClass());

	private final CommandExecutorRegistry executorRegistry = new CommandExecutorRegistryImpl();

	protected void injectService(Class<?> serviceInterface, Object service) {
		this.executorRegistry.injectService(serviceInterface, service);
	}

	@Override
	public void init(ServletConfig config) throws ServletException {
		super.init(config);
		String servicesParam = config.getInitParameter(BasicCommandService.PARAM_SERVICES);
		if (servicesParam != null) {
			String[] serviceToInstanciate = servicesParam.split(";");
			if (serviceToInstanciate != null && serviceToInstanciate.length > 0) {
				for (String serviceName : serviceToInstanciate) {
					if (serviceName != null && serviceName.length() > 0) {
						try {
							this.getClass();
							Class<?> serviceClass = Class.forName(serviceName);
							Object service = serviceClass.newInstance();
							for (Class<?> serviceInterface : serviceClass.getInterfaces()) {
								this.injectService(serviceInterface, service);
							}
						} catch (ClassNotFoundException e) {
							throw new ServletException("Can not load service class " + serviceName, e);
						} catch (InstantiationException e) {
							throw new ServletException("Can not instantiate service " + serviceName, e);
						} catch (IllegalAccessException e) {
							throw new ServletException("Can not instantiate service " + serviceName, e);
						}
					}
				}
			}
		}
	}

	@Override
	public List<CommandResponse> executeCommands(List<CommandRequest> commands) {
		List<CommandResponse> result = Lists.newArrayList();

		for (CommandRequest request : commands) {
			CommandExecutor executor = this.executorRegistry.resolveCommandExecutor(request.getCommandDefinition());
			result.add(executor.executeCommand(request));
		}

		return result;
	}

	@Override
	public SerializationPolicy getSerializationPolicy(String moduleBaseURL, String serializationPolicyStrongName) {
		return CommandSerializationPolicy.get();
	}

	@Override
	protected void processPost(HttpServletRequest request, HttpServletResponse response) throws Throwable {
		try {
			String requestPayload = this.readContent(request);
			RPCRequest rpcRequest = RPC.decodeRequest(requestPayload, this.getClass(), this);

			String responsePayload =
				RPC.invokeAndEncodeResponse(this, rpcRequest.getMethod(), rpcRequest.getParameters(),
					rpcRequest.getSerializationPolicy(), rpcRequest.getFlags());

			boolean gzipEncode =
				RPCServletUtils.acceptsGzipEncoding(request)
					&& RPCServletUtils.exceedsUncompressedContentLengthLimit(responsePayload);

			RPCServletUtils.writeResponse(null, response, responsePayload, gzipEncode);
		} catch (Exception e) {
			this.logger.error("Request processing failed", e);
			throw Throwables.propagate(e);
		}
	}
}
