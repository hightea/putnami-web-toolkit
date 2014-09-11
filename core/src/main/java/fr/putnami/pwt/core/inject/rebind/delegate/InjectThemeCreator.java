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

import java.util.List;

import com.google.common.collect.Lists;
import com.google.gwt.core.client.EntryPoint;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.client.annotation.ThemeDescription;
import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterEntryPoint;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterInit;
import fr.putnami.pwt.core.theme.client.CssLink;
import fr.putnami.pwt.core.theme.client.Theme;
import fr.putnami.pwt.core.theme.client.ThemeController;

public class InjectThemeCreator extends InjectorCreatorDelegate implements InjectorWritterInit, InjectorWritterEntryPoint {

	private final List<String> styles = Lists.newArrayList();

	public InjectThemeCreator(JClassType injectableType, ThemeDescription themeDescritpion) {
		if (themeDescritpion.styleSheets() != null) {
			for (String style : themeDescritpion.styleSheets()) {
				styles.add(style);
			}

		}
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(EntryPoint.class.getName());
		composerFactory.addImport(Theme.class.getName());
		composerFactory.addImport(ThemeController.class.getName());
		composerFactory.addImport(CssLink.class.getName());

		composerFactory.addImplementedInterface(EntryPoint.class.getName());
	}

	@Override
	public void writeEntryPoint(SourceWriter srcWriter) {
		srcWriter.println("Theme theme = new Theme();");
		for (String style : styles) {
			srcWriter.println("theme.addLink(new CssLink(\"%s\", 0));", style);
		}
		srcWriter.println("ThemeController.get().installTheme(theme);");
	}

}
