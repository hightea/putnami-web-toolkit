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
package fr.putnami.pwt.core.inject.rebind.util;

import com.google.common.collect.Lists;
import com.google.gwt.core.ext.typeinfo.JClassType;
import com.google.gwt.core.ext.typeinfo.JField;
import com.google.gwt.core.ext.typeinfo.JMethod;

import java.lang.annotation.Annotation;
import java.util.Collection;

public final class InjectCreatorUtil {

	public static Collection<JMethod> listMethod(JClassType type,
			Class<? extends Annotation> annotationClass) {
		Collection<JMethod> methodAnnoted = Lists.newArrayList();
		JMethod[] methods = type.getOverridableMethods();
		for (JMethod method : methods) {
			Annotation annotation = method.getAnnotation(annotationClass);
			if (annotation != null) {
				methodAnnoted.add(method);
			}
		}

		return methodAnnoted;
	}

	public static Collection<JField> listFields(JClassType type,
			Class<? extends Annotation> annotationClass) {
		Collection<JField> methodAnnoted = Lists.newArrayList();
		JField[] fields = type.getFields();
		for (JField field : fields) {
			Annotation annotation = field.getAnnotation(annotationClass);
			if (annotation != null) {
				methodAnnoted.add(field);
			}
		}
		// Recurse to superclass
		JClassType superclass = type.getSuperclass();
		if (superclass != null) {
			methodAnnoted.addAll(InjectCreatorUtil.listFields(superclass, annotationClass));
		}

		return methodAnnoted;
	}

	public static String toClassName(Class<?> clazz) {
		return clazz == null ? null : clazz.getName().replace('$', '.');
	}

	private InjectCreatorUtil() {
	}
}
