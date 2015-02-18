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
package fr.putnami.pwt.plugin.spring.rpc.server.controller;

import com.google.common.base.Throwables;
import com.google.gwt.user.server.rpc.RPC;
import com.google.gwt.user.server.rpc.RPCRequest;
import com.google.gwt.user.server.rpc.RPCServletUtils;
import com.google.gwt.user.server.rpc.SerializationPolicy;
import com.google.gwt.user.server.rpc.SerializationPolicyProvider;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import fr.putnami.pwt.core.service.server.service.CommandSerializationPolicy;
import fr.putnami.pwt.core.service.shared.service.CommandService;

@Controller
public class CommandController implements SerializationPolicyProvider {

	private final Log logger = LogFactory.getLog(this.getClass());

	@Autowired
	private CommandService commandService;

	@RequestMapping(value = "/commandService", method = RequestMethod.POST)
	public void processPostRpc(HttpServletRequest request, HttpServletResponse response)
		throws Throwable {
		try {
			String requestPayload = RPCServletUtils.readContentAsGwtRpc(request);
			RPCRequest rpcRequest = RPC.decodeRequest(requestPayload, CommandService.class, this);

			String responsePayload =
				RPC.invokeAndEncodeResponse(commandService,
					rpcRequest.getMethod(), rpcRequest.getParameters(),
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

	@Override
	public SerializationPolicy getSerializationPolicy(String moduleBaseURL, String serializationPolicyStrongName) {
		return CommandSerializationPolicy.get();
	}

}
