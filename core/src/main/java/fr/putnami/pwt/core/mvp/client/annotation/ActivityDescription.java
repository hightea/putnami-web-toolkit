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
package fr.putnami.pwt.core.mvp.client.annotation;

import com.google.gwt.place.shared.PlaceTokenizer;
import com.google.gwt.user.client.ui.IsWidget;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import fr.putnami.pwt.core.mvp.client.ViewDecorator;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface ActivityDescription {

	Class<? extends IsWidget> view();

	Class<? extends ViewDecorator> viewDecorator() default ViewDecorator.class;

	Class<? extends PlaceTokenizer> placeTokenizer() default PlaceTokenizer.class;

	boolean asyncView() default true;

	String[] aliases() default {};

	Scope scope() default Scope.SINGLETON;

	enum Scope {
			SINGLETON,
			PROTOTYPE
	}
}
