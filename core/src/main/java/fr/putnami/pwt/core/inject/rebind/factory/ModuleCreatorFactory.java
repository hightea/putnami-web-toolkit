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
package fr.putnami.pwt.core.inject.rebind.factory;

import java.util.Collection;

import com.google.gwt.core.ext.typeinfo.JClassType;

import fr.putnami.pwt.core.inject.client.annotation.ErrorManagmentDescription;
import fr.putnami.pwt.core.inject.client.annotation.MvpDescription;
import fr.putnami.pwt.core.inject.client.annotation.ThemeDescription;
import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorDelegateFactorty;
import fr.putnami.pwt.core.inject.rebind.delegate.InjectErrorManagerCreator;
import fr.putnami.pwt.core.inject.rebind.delegate.InjectMvpDescriptionCreator;
import fr.putnami.pwt.core.inject.rebind.delegate.InjectThemeCreator;

public class ModuleCreatorFactory implements InjectorDelegateFactorty {

	@Override
	public void createDelegates(JClassType injectableType, Collection<InjectorCreatorDelegate> delegates) {
		if (injectableType.getAnnotation(MvpDescription.class) != null) {
			delegates.add(new InjectMvpDescriptionCreator(injectableType, injectableType.getAnnotation(MvpDescription.class)));
		}
		if (injectableType.getAnnotation(ThemeDescription.class) != null) {
			delegates.add(new InjectThemeCreator(injectableType, injectableType.getAnnotation(ThemeDescription.class)));
		}
		if (injectableType.getAnnotation(ErrorManagmentDescription.class) != null) {
			delegates.add(new InjectErrorManagerCreator(injectableType, injectableType.getAnnotation(ErrorManagmentDescription.class)));
		}
	}

}
