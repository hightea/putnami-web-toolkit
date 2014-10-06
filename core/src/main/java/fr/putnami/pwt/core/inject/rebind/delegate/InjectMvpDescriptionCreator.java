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

import com.google.common.collect.Lists;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.place.shared.Place;
import com.google.gwt.user.client.ui.AcceptsOneWidget;
import com.google.gwt.user.client.ui.IsWidget;
import com.google.gwt.user.client.ui.RootPanel;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import java.util.List;

import static fr.putnami.pwt.core.inject.rebind.util.InjectCreatorUtil.toClassName;

import fr.putnami.pwt.core.inject.client.annotation.MvpDescription;
import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterEntryPoint;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterInit;
import fr.putnami.pwt.core.mvp.client.ActivityFactory;
import fr.putnami.pwt.core.mvp.client.MvpController;

public class InjectMvpDescriptionCreator extends InjectorCreatorDelegate implements InjectorWritterInit, InjectorWritterEntryPoint {

	private final Class<? extends AcceptsOneWidget> display;
	private final Class<? extends Place> defaultPlace;
	private final List<Class<?>> activities = Lists.newArrayList();

	public InjectMvpDescriptionCreator(JClassType injectableType, MvpDescription moduleDescription) {
		this.display = moduleDescription.display();
		this.defaultPlace = moduleDescription.defaultPlace();
		if (moduleDescription.activities() != null) {
			for (Class<?> activity : moduleDescription.activities()) {
				activities.add(activity);
			}

		}
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(EntryPoint.class.getName());
		composerFactory.addImport(MvpController.class.getName());
		composerFactory.addImport(ActivityFactory.class.getName());
		composerFactory.addImport(EntryPoint.class.getName());
		composerFactory.addImport(AcceptsOneWidget.class.getName());
		composerFactory.addImport(IsWidget.class.getName());
		composerFactory.addImport(RootPanel.class.getName());

		composerFactory.addImplementedInterface(EntryPoint.class.getName());
	}

	@Override
	public void writeEntryPoint(SourceWriter srcWriter) {
		srcWriter.println("MvpController mvpController = MvpController.get();");
		srcWriter.println("AcceptsOneWidget mvpDisplay = null;");
		if (display != null && !AcceptsOneWidget.class.equals(display)) {
			srcWriter.println("mvpDisplay = GWT.create(%s.class);", toClassName(display));
		}
		srcWriter.println("if(mvpDisplay != null){");
		srcWriter.indent();
		srcWriter.println("mvpController.setDisplay(mvpDisplay);");
		srcWriter.outdent();
		srcWriter.println("}");
		srcWriter.println("if(mvpDisplay instanceof IsWidget){");
		srcWriter.indent();
		srcWriter.println("RootPanel.get().add((IsWidget) mvpDisplay);");
		srcWriter.outdent();
		srcWriter.println("}");

		if (defaultPlace != null && !Place.class.equals(defaultPlace)) {
			srcWriter.println("mvpController.setDefaultPlace(new %s());", toClassName(defaultPlace));
		}
		for (Class<?> activity : activities) {
			srcWriter.println("mvpController.registerActivity(GWT.<ActivityFactory> create(%s.class));", toClassName(activity));
		}
	}

}
