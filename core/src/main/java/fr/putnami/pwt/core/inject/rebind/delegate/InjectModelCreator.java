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
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JField;
import com.google.gwt.core.ext.typeinfo.JParameterizedType;
import com.google.gwt.core.ext.typeinfo.JType;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterConstructor;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterInit;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterSubGenerate;
import fr.putnami.pwt.core.model.client.model.Model;
import fr.putnami.pwt.core.model.rebind.ModelCreator;

public class InjectModelCreator extends InjectorCreatorDelegate implements InjectorWritterSubGenerate, InjectorWritterConstructor,
		InjectorWritterInit {

	private final JField modelField;
	private final JType fieldType;
	private final JClassType beanType;

	private String modelImplClass;

	public InjectModelCreator(JField modelField) {
		this.modelField = modelField;
		this.fieldType = modelField.getType();

		if (this.fieldType instanceof JParameterizedType) {
			JParameterizedType paramType = (JParameterizedType) fieldType;
			beanType = paramType.getTypeArgs()[0];
		}
		else {
			throw new RuntimeException("modelField can not be injected as Model");
		}

	}

	@Override
	public void subGenerate(TreeLogger logger, GeneratorContext context) {
		ModelCreator modelCreator = new ModelCreator(beanType);
		modelImplClass = modelCreator.create(logger, context);
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(Model.class.getName());
		composerFactory.addImport(beanType.getQualifiedSourceName());

	}

	@Override
	public void writeConstructor(SourceWriter srcWriter) {
		srcWriter.println("%s = new %s();", modelField.getName(), modelImplClass);
	}
}
