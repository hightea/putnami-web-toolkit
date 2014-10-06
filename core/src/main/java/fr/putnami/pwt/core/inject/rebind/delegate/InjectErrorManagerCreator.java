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
package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JMethod;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import java.util.Collection;

import static fr.putnami.pwt.core.inject.rebind.util.InjectCreatorUtil.toClassName;

import fr.putnami.pwt.core.error.client.ErrorDisplayer;
import fr.putnami.pwt.core.error.client.ErrorHandler;
import fr.putnami.pwt.core.error.client.ErrorManager;
import fr.putnami.pwt.core.error.client.widget.SimpleErrorDisplayer;
import fr.putnami.pwt.core.inject.client.annotation.ErrorManagmentDescription;
import fr.putnami.pwt.core.inject.rebind.InjectorViewCreator;
import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterEntryPoint;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterInit;
import fr.putnami.pwt.core.inject.rebind.util.InjectCreatorUtil;

public class InjectErrorManagerCreator extends InjectorCreatorDelegate implements InjectorWritterInit, InjectorWritterEntryPoint {

	private final Class<? extends ErrorDisplayer> errorDisplay;
	private final Class<? extends ErrorHandler>[] errorHandlers;
	private final Collection<JMethod> handlerMethods;
	private final String injectorName;

	public InjectErrorManagerCreator(JClassType injectableType, ErrorManagmentDescription errorManagmentDescritpion) {
		this.injectorName = injectableType.getSimpleSourceName() + InjectorViewCreator.PROXY_SUFFIX;
		this.errorDisplay = errorManagmentDescritpion.errorDisplay();
		this.errorHandlers = errorManagmentDescritpion.errorHandlers();
		this.handlerMethods = InjectCreatorUtil.listMethod(injectableType, fr.putnami.pwt.core.inject.client.annotation.ErrorHandler.class);
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(EntryPoint.class.getName());
		composerFactory.addImport(SimpleErrorDisplayer.class.getName());
		composerFactory.addImport(ErrorManager.class.getName());
		composerFactory.addImport(ErrorDisplayer.class.getName());

		composerFactory.addImplementedInterface(EntryPoint.class.getName());
	}

	@Override
	public void writeEntryPoint(SourceWriter srcWriter) {
		if (errorDisplay != null) {
			srcWriter.println("ErrorManager.get().setErrorDisplayer(GWT.<ErrorDisplayer> create(%s.class));", toClassName(errorDisplay));
		}
		if (errorHandlers != null) {
			for (Class<? extends ErrorHandler> handlerClass : errorHandlers) {
				srcWriter.println("ErrorManager.get().registerErrorHandler(new %s());", toClassName(handlerClass));
			}
		}
		if (errorHandlers != null) {
			for (JMethod handlerMethod : handlerMethods) {
				srcWriter.println("ErrorManager.get().registerErrorHandler(new fr.putnami.pwt.core.error.client.ErrorHandler() {");
				srcWriter.indent();
				srcWriter.println("@Override public boolean handle(Throwable error) { "
						+ "return %s.this.%s(error); "
						+ "}", injectorName, handlerMethod.getName());
				srcWriter.println("@Override public int getPriority() { return HIGH_PRIORITY; }");
				srcWriter.outdent();
				srcWriter.println("});");
			}
		}
	}

}
