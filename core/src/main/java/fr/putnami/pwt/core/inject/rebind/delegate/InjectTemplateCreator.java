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

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.uibinder.client.UiTemplate;
import com.google.gwt.user.client.ui.Widget;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.client.annotation.Templated;
import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterConstructor;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterInit;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterStatic;
import fr.putnami.pwt.core.widget.client.binder.UiBinderLocalized;

public class InjectTemplateCreator extends InjectorCreatorDelegate implements InjectorWritterStatic, InjectorWritterInit, InjectorWritterConstructor {

	private final JClassType viewType;
	private final String templateInterfaceName;
	private final String templateName;

	public InjectTemplateCreator(JClassType viewType) {
		this.viewType = viewType;
		this.templateInterfaceName = viewType.getSimpleSourceName() + "TemplateBinder";
		Templated templateAnnotation = viewType.getAnnotation(Templated.class);
		if (Templated.DEFAULT_VALUE.equals(templateAnnotation.value())) {
			templateName = null;
		}
		else {
			templateName = templateAnnotation.value();
		}

	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(UiBinderLocalized.class.getName());
		composerFactory.addImport(Widget.class.getName());
		composerFactory.addImport(UiTemplate.class.getName());
	}

	@Override
	public void writeStatic(SourceWriter srcWriter) {
		if (templateName != null) {
			srcWriter.println("@UiTemplate(%s)", templateName);
		}
		srcWriter.println("interface %s extends UiBinderLocalized<Widget, %s> {}", templateInterfaceName, viewType.getSimpleSourceName());
	}

	@Override
	public void writeConstructor(SourceWriter srcWriter) {
		srcWriter.println("initWidget(((%s)GWT.create(%s.class)).createAndBindUi(this));", templateInterfaceName, templateInterfaceName);
	}

	@Override
	public int getOrder() {
		return 1;
	}
}
