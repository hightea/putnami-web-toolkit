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

import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterInit;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterMethod;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterPresent;

public class InjectDecoratorPresenterCreator extends InjectorCreatorDelegate implements
InjectorWritterMethod, InjectorWritterPresent, InjectorWritterInit {

	public InjectDecoratorPresenterCreator() {
	}

	@Override
	public int getOrder() {
		return InjectorCreatorDelegate.LOWEST_PRECEDENCE;
	}

	@Override
	public void writePresent(SourceWriter srcWriter) {
		srcWriter.println("presentDecoratedView(place);");
	}

	@Override
	public void writeMethods(SourceWriter srcWriter) {
		srcWriter.println("public void presentDecoratedView(Place place) {");
		srcWriter.indent();
		srcWriter.println("if (view instanceof Presenter) {");
		srcWriter.indent();
		srcWriter.println("Presenter presenter = (Presenter) view;");
		srcWriter.println("presenter.present(place, this);");
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.outdent();
		srcWriter.println("}");
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(AcceptsOneWidget.class.getName());
	}

}
