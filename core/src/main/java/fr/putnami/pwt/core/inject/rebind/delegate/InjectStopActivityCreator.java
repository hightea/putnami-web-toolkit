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

import java.util.Collection;

import com.google.gwt.core.ext.typeinfo.JMethod;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.event.client.EventBus;
import fr.putnami.pwt.core.event.client.HandlerRegistrationCollection;
import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.mvp.client.event.StopActivityEvent;

public class InjectStopActivityCreator extends InjectorCreatorDelegate {

	private final Collection<JMethod> presenterMethods;
	private final String injectorName;

	public InjectStopActivityCreator(Collection<JMethod> presenterMethods, String injectorName) {
		this.presenterMethods = presenterMethods;
		this.injectorName = injectorName;
	}

	@Override
	public int getOrder() {
		return LOWEST_PRECEDENCE;
	}

	@Override
	public void writePresent(SourceWriter srcWriter) {
		srcWriter.println("final HandlerRegistrationCollection stopRegistrations = new HandlerRegistrationCollection();");
		for (JMethod mayStopMethod : presenterMethods) {
			srcWriter.println("stopRegistrations.add(EventBus.get()"
					+ ".addHandlerToSource(StopActivityEvent.TYPE, place, new StopActivityEvent.Handler() {");
			srcWriter.indent();
			srcWriter.println("@Override public void onStopActivity(StopActivityEvent event) {");
			srcWriter.indent();
			srcWriter.println("%s.this.%s();", injectorName, mayStopMethod.getName());
			srcWriter.outdent();
			srcWriter.outdent();
			srcWriter.println("}}));");
		}
		srcWriter.println("stopRegistrations.add(EventBus.get()"
				+ ".addHandlerToSource(StopActivityEvent.TYPE, place, new StopActivityEvent.Handler() {");
		srcWriter.indent();
		srcWriter.println("@Override public void onStopActivity(StopActivityEvent event) {");
		srcWriter.indent();
		srcWriter.println("stopRegistrations.removeHandler();");
		srcWriter.outdent();
		srcWriter.outdent();
		srcWriter.println("}}));");
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(EventBus.class.getName());
		composerFactory.addImport(HandlerRegistrationCollection.class.getName());
		composerFactory.addImport(StopActivityEvent.class.getName());
	}

}