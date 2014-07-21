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

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.gwt.user.server.rpc.AbstractRemoteServiceServlet;
import com.google.gwt.user.server.rpc.RPC;
import com.google.gwt.user.server.rpc.RPCRequest;
import com.google.gwt.user.server.rpc.RPCServletUtils;
import com.google.gwt.user.server.rpc.SerializationPolicy;
import com.google.gwt.user.server.rpc.SerializationPolicyProvider;

import fr.putnami.pwt.core.service.shared.domain.CommandRequest;
import fr.putnami.pwt.core.service.shared.domain.CommandResponse;
import fr.putnami.pwt.core.service.shared.service.CommandService;

public abstract class AbstractCommandService extends AbstractRemoteServiceServlet implements CommandService, SerializationPolicyProvider {
	private final Log logger = LogFactory.getLog(getClass());

	private final CommandExecutorRegistry executorRegistry = new CommandExecutorRegistryImpl();

	protected void injectService(Class<?> serviceInterface, Object service) {
		executorRegistry.injectService(serviceInterface, service);
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
			RPCRequest rpcRequest = RPC.decodeRequest(requestPayload, getClass(), this);

			String responsePayload = RPC.invokeAndEncodeResponse(this,
					rpcRequest.getMethod(), rpcRequest.getParameters(), rpcRequest.getSerializationPolicy(), rpcRequest.getFlags());

			boolean gzipEncode = RPCServletUtils.acceptsGzipEncoding(request)
					&& RPCServletUtils.exceedsUncompressedContentLengthLimit(responsePayload);

			RPCServletUtils.writeResponse(null, response, responsePayload, gzipEncode);
		}
		catch (Exception e) {
			logger.error("Request processing failed", e);
			throw Throwables.propagate(e);
		}
	}
}
