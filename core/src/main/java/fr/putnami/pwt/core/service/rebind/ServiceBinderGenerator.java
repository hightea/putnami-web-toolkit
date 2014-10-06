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
package fr.putnami.pwt.core.service.rebind;

import com.google.gwt.core.ext.Generator;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.UnableToCompleteException;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JParameterizedType;
import com.google.gwt.core.ext.typeinfo.NotFoundException;
import com.google.gwt.core.ext.typeinfo.TypeOracle;

import fr.putnami.pwt.core.service.client.ServiceProxy;

public class ServiceBinderGenerator extends Generator {

	@Override
	public String generate(TreeLogger logger, GeneratorContext context, String remoteServiceClassName)
			throws UnableToCompleteException {
		TypeOracle typeOracle = context.getTypeOracle();
		assert (typeOracle != null);

		JClassType remoteServiceType = typeOracle.findType(remoteServiceClassName);
		if (remoteServiceType == null) {
			logger.log(TreeLogger.ERROR, "Unable to find metadata for type '" + remoteServiceClassName
					+ "'", null);
			throw new UnableToCompleteException();
		}

		JClassType handlerType = null;
		JClassType serviceType = null;
		for (JClassType interfaceType : remoteServiceType.getImplementedInterfaces()) {
			if (interfaceType.getQualifiedSourceName().equals(ServiceProxy.class.getCanonicalName())
					&& interfaceType instanceof JParameterizedType) {
				JParameterizedType paramType = (JParameterizedType) interfaceType;
				handlerType = paramType.getTypeArgs()[0];
				serviceType = paramType.getTypeArgs()[1];
			}
		}
		if (serviceType == null) {
			logger.log(TreeLogger.ERROR, remoteServiceType.getQualifiedSourceName() + " must implement "
					+ ServiceProxy.class.getCanonicalName(), null);
			throw new UnableToCompleteException();
		}

		ServiceBinderCreator serviceBinderCreator =
				new ServiceBinderCreator(remoteServiceType, serviceType, handlerType);
		try {
			return serviceBinderCreator.create(logger, context);
		} catch (NotFoundException e) {
			throw new UnableToCompleteException();
		}
	}

}
