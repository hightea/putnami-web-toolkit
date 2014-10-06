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
package fr.putnami.pwt.core.model.rebind;

import com.google.gwt.core.ext.Generator;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.UnableToCompleteException;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JParameterizedType;
import com.google.gwt.core.ext.typeinfo.TypeOracle;

import fr.putnami.pwt.core.model.client.model.Model;

public class ModelGenerator extends Generator {

	@Override
	public String generate(TreeLogger logger, GeneratorContext context, String modelClass)
			throws UnableToCompleteException {
		TypeOracle typeOracle = context.getTypeOracle();
		assert (typeOracle != null);

		JClassType modelType = typeOracle.findType(modelClass);
		if (modelType == null) {
			logger.log(TreeLogger.ERROR, "Unable to find metadata for type '" + modelClass + "'", null);
			throw new UnableToCompleteException();
		}

		JClassType beanType = null;
		for (JClassType interfaceType : modelType.getImplementedInterfaces()) {
			if (interfaceType.getQualifiedSourceName().equals(Model.class.getCanonicalName())
					&& interfaceType instanceof JParameterizedType) {
				JParameterizedType paramType = (JParameterizedType) interfaceType;
				beanType = paramType.getTypeArgs()[0];
			}
		}
		if (beanType == null) {
			logger.log(TreeLogger.ERROR, modelType.getQualifiedSourceName() + " must implement "
					+ Model.class.getCanonicalName(), null);
			throw new UnableToCompleteException();
		}

		ModelCreator modelCreator = new ModelCreator(beanType);
		return modelCreator.create(logger, context);
	}

}
