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
package fr.putnami.pwt.core.service.shared.service;

import java.util.List;

import com.google.gwt.user.client.rpc.RemoteService;
import com.google.gwt.user.client.rpc.RemoteServiceRelativePath;

import fr.putnami.pwt.core.service.shared.domain.CommandRequest;
import fr.putnami.pwt.core.service.shared.domain.CommandResponse;

@RemoteServiceRelativePath("commandService")
public interface CommandService extends RemoteService {

	String EXCEPTION_ILLEGAL_ARGUMENT = "EXCEPTION_ILLEGAL_ARGS";
	String EXCEPTION_ILLEGAL_ACCESS = "EXCEPTION_ILLEGAL_ACCESS";
	String EXCEPTION_INVOKATION = "EXCEPTION_INVOKATION";

	List<CommandResponse> executeCommands(List<CommandRequest> commands);
}
