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
package fr.putnami.pwt.plugin.rest.rebind;

import com.google.gwt.core.ext.Generator;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.UnableToCompleteException;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JParameterizedType;
import com.google.gwt.core.ext.typeinfo.TypeOracle;

import fr.putnami.pwt.core.service.client.ServiceProxy;

public class RestServiceBinderGenerator extends Generator {

	@Override
	public String generate(TreeLogger logger, GeneratorContext context, String restServiceClassName)
		throws UnableToCompleteException {
		TypeOracle typeOracle = context.getTypeOracle();
		assert typeOracle != null;

		JClassType restServiceType = typeOracle.findType(restServiceClassName);
		if (restServiceType == null) {
			logger.log(TreeLogger.ERROR, "Unable to find metadata for type '" + restServiceClassName + "'", null);
			throw new UnableToCompleteException();
		}

		JClassType handlerType = null;
		JClassType serviceType = null;
		for (JClassType interfaceType : restServiceType.getImplementedInterfaces()) {
			if (interfaceType.getQualifiedSourceName().equals(ServiceProxy.class.getCanonicalName())
				&& interfaceType instanceof JParameterizedType) {
				JParameterizedType paramType = (JParameterizedType) interfaceType;
				handlerType = paramType.getTypeArgs()[0];
				serviceType = paramType.getTypeArgs()[1];
			}
		}
		if (serviceType == null) {
			logger.log(TreeLogger.ERROR,
				restServiceType.getQualifiedSourceName() + " must implement " + ServiceProxy.class.getCanonicalName()
					+ " with explicit Type Parameters", null);
			throw new UnableToCompleteException();
		}

		RestServiceBinderCreator serviceBinderCreator =
			new RestServiceBinderCreator(restServiceType, serviceType, handlerType);
		return serviceBinderCreator.create(logger, context);
	}

}
