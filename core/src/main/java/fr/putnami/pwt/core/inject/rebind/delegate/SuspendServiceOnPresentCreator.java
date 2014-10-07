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
package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import java.util.List;

import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterAfterPresent;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterBeforePresent;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterInit;
import fr.putnami.pwt.core.service.client.CallbackAdapter;
import fr.putnami.pwt.core.service.client.CommandController;
import fr.putnami.pwt.core.service.shared.domain.CommandResponse;

public class SuspendServiceOnPresentCreator extends InjectorCreatorDelegate
	implements InjectorWritterAfterPresent, InjectorWritterBeforePresent, InjectorWritterInit {

	private final String injectorName;
	private boolean hasService;

	public SuspendServiceOnPresentCreator(String injectorName, boolean hasService) {
		this.injectorName = injectorName;
		this.hasService = hasService;
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		if (this.hasService) {
			composerFactory.addImport(CommandController.class.getName());
			composerFactory.addImport(CallbackAdapter.class.getName());
			composerFactory.addImport(List.class.getName());
			composerFactory.addImport(CommandResponse.class.getName());
		}
	}

	@Override
	public void writeBeforePresent(SourceWriter srcWriter) {
		if (this.hasService) {
			srcWriter.println("CommandController commandController = CommandController.get();");
			srcWriter.println("boolean isSuspended = commandController.isSuspended();");
			srcWriter.println("commandController.setSuspended(true);");
		}
	}

	@Override
	public void writeAfterPresent(SourceWriter srcWriter) {
		if (!this.hasService) {
			srcWriter.println("displayer.setWidget(%s.this);", this.injectorName);
		} else {
			srcWriter.println("commandController.flush(new CallbackAdapter<List<CommandResponse>>() {");
			srcWriter.indent();
			srcWriter.println("@Override");
			srcWriter.println("public void onSuccess(List<CommandResponse> result) {");
			srcWriter.indent();
			srcWriter.println("displayer.setWidget(%s.this);", this.injectorName);
			srcWriter.outdent();
			srcWriter.println("};");
			srcWriter.outdent();
			srcWriter.println("});");
			srcWriter.println("commandController.setSuspended(isSuspended);");
		}
	}
}
