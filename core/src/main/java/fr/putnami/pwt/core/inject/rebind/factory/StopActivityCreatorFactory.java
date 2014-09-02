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
import com.google.gwt.core.ext.typeinfo.JMethod;

import fr.putnami.pwt.core.inject.client.annotation.StopActivityHandler;
import fr.putnami.pwt.core.inject.rebind.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.InjectorDelegateFactorty;
import fr.putnami.pwt.core.inject.rebind.InjectorProxyCreator;
import fr.putnami.pwt.core.inject.rebind.delegate.InjectStopActivityCreator;
import fr.putnami.pwt.core.inject.rebind.util.InjectCreatorUtil;

public class StopActivityCreatorFactory implements InjectorDelegateFactorty {

	@Override
	public void createDelegates(JClassType injectableType, Collection<InjectorCreatorDelegate> delegates) {
		Collection<JMethod> methods = InjectCreatorUtil.listMethod(injectableType, StopActivityHandler.class);
		if (!methods.isEmpty()) {
			String injectorName = injectableType.getSimpleSourceName() + InjectorProxyCreator.PROXY_SUFFIX;
			delegates.add(new InjectStopActivityCreator(methods, injectorName));
		}
	}

}
