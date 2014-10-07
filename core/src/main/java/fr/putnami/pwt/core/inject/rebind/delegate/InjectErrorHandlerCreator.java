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

import com.google.common.collect.Lists;
import com.google.gwt.core.ext.typeinfo.JMethod;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import java.util.Collection;
import java.util.List;

import fr.putnami.pwt.core.error.client.ErrorManager;
import fr.putnami.pwt.core.event.client.EventBus;
import fr.putnami.pwt.core.event.client.HandlerRegistrationCollection;
import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterInit;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterPresent;
import fr.putnami.pwt.core.mvp.client.event.StopActivityEvent;

public class InjectErrorHandlerCreator extends InjectorCreatorDelegate
	implements InjectorWritterInit, InjectorWritterPresent {

	private final Collection<JMethod> presenterMethods;
	private final String injectorName;

	public InjectErrorHandlerCreator(Collection<JMethod> presenterMethods, String injectorName) {
		this.presenterMethods = presenterMethods;
		this.injectorName = injectorName;
	}

	@Override
	public int getOrder() {
		return InjectorCreatorDelegate.LOWEST_PRECEDENCE;
	}

	@Override
	public void writePresent(SourceWriter srcWriter) {

		srcWriter
			.println("final List<fr.putnami.pwt.core.error.client.ErrorHandler> errorHandlers = Lists.newArrayList();");
		for (JMethod handlerMethod : this.presenterMethods) {
			srcWriter.println("errorHandlers.add(new fr.putnami.pwt.core.error.client.ErrorHandler() {");
			srcWriter.indent();
			srcWriter.println("@Override public boolean handle(Throwable error) { "
				+ "return %s.this.%s(error); " + "}", this.injectorName, handlerMethod.getName());
			srcWriter.println("@Override public int getPriority() { return HIGH_PRIORITY; }");
			srcWriter.outdent();
			srcWriter.println("});");
		}

		srcWriter
			.println("for (fr.putnami.pwt.core.error.client.ErrorHandler errorHandler : errorHandlers) {");
		srcWriter.indent();
		srcWriter.println("ErrorManager.get().registerErrorHandler(errorHandler);");
		srcWriter.outdent();
		srcWriter.println("}");

		srcWriter
			.println("final HandlerRegistrationCollection errorHandlerRegistrations = new HandlerRegistrationCollection();");
		srcWriter
			.println("errorHandlerRegistrations.add(EventBus.get().addHandlerToSource(StopActivityEvent.TYPE, place, new StopActivityEvent.Handler() {");
		srcWriter.indent();
		srcWriter.println("@Override public void onStopActivity(StopActivityEvent event) {");
		srcWriter.indent();
		srcWriter
			.println("for (fr.putnami.pwt.core.error.client.ErrorHandler handler : errorHandlers) {");
		srcWriter.indent();
		srcWriter.println("ErrorManager.get().registerErrorHandler(handler);");
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.outdent();
		srcWriter.println("errorHandlerRegistrations.removeHandler();");
		srcWriter.outdent();
		srcWriter.println("}}));");
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(EventBus.class.getName());
		composerFactory.addImport(HandlerRegistrationCollection.class.getName());
		composerFactory.addImport(StopActivityEvent.class.getName());
		composerFactory.addImport(ErrorManager.class.getName());
		composerFactory.addImport(List.class.getName());
		composerFactory.addImport(Lists.class.getName());
		composerFactory.addImport(Lists.class.getName());
		composerFactory.addImport(Lists.class.getName());
	}

}
