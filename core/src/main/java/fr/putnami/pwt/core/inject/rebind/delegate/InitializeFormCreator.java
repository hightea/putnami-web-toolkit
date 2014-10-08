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
package fr.putnami.pwt.core.inject.rebind.delegate;

import com.google.gwt.core.client.GWT;
import com.google.gwt.core.ext.GeneratorContext;
import com.google.gwt.core.ext.TreeLogger;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JField;
import com.google.gwt.core.ext.typeinfo.JParameterizedType;
import com.google.gwt.core.ext.typeinfo.JType;
import com.google.gwt.i18n.client.ConstantsWithLookup;
import com.google.gwt.user.rebind.ClassSourceFileComposerFactory;
import com.google.gwt.user.rebind.SourceWriter;

import fr.putnami.pwt.core.editor.client.helper.MessageHelper;
import fr.putnami.pwt.core.inject.client.annotation.Initialize;
import fr.putnami.pwt.core.inject.rebind.base.InjectorCreatorDelegate;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterConstructor;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterInit;
import fr.putnami.pwt.core.inject.rebind.base.InjectorWritterSubGenerate;
import fr.putnami.pwt.core.model.rebind.ModelCreator;

public class InitializeFormCreator extends InjectorCreatorDelegate
	implements InjectorWritterSubGenerate, InjectorWritterConstructor, InjectorWritterInit {

	private final JField modelField;
	private final JType fieldType;
	private final JClassType beanType;
	private String modelImplClass;
	private Class<? extends ConstantsWithLookup> constantClassName;

	public InitializeFormCreator(JField modelField) {
		this.modelField = modelField;
		this.fieldType = modelField.getType();

		Initialize initializeAnnotation = modelField.getAnnotation(Initialize.class);
		this.constantClassName = initializeAnnotation.constantsClass();
		if (ConstantsWithLookup.class.equals(this.constantClassName)) {
			this.constantClassName = null;
		}
		if (this.fieldType instanceof JParameterizedType) {
			JParameterizedType paramType = (JParameterizedType) this.fieldType;
			this.beanType = paramType.getTypeArgs()[0];
		} else {
			throw new RuntimeException("modelField can not be injected as Model");
		}
	}

	@Override
	public int getOrder() {
		return 2;
	}

	@Override
	public void subGenerate(TreeLogger logger, GeneratorContext context) {
		ModelCreator modelCreator = new ModelCreator(this.beanType);
		this.modelImplClass = modelCreator.create(logger, context);
	}

	@Override
	public void initComposer(ClassSourceFileComposerFactory composerFactory) {
		composerFactory.addImport(GWT.class.getName());
		composerFactory.addImport(MessageHelper.class.getName());
		composerFactory.addImport(ConstantsWithLookup.class.getName());
		composerFactory.addImport(this.beanType.getQualifiedSourceName());
	}

	@Override
	public void writeConstructor(SourceWriter srcWriter) {
		String fieldName = this.modelField.getName();
		if (this.constantClassName != null) {
			srcWriter.println(
				"MessageHelper %sMessageHelper = new MessageHelper((ConstantsWithLookup) GWT.create(%s.class));",
				fieldName, this.constantClassName.getCanonicalName());
			srcWriter.println("%s.setMessageHelper(%sMessageHelper);", fieldName, fieldName);
		}
		srcWriter.println("%s.initialize(new %s());", fieldName, this.modelImplClass);
	}
}
