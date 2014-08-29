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
package fr.putnami.pwt.doc.client.page.tutorial;

import com.google.gwt.core.client.GWT;

import fr.putnami.pwt.core.mvp.client.MvpPlace;
import fr.putnami.pwt.core.mvp.client.ViewProxy;

public class Tuto3MorePagesPlace extends MvpPlace {

	public static final Tuto3MorePagesPlace INSTANCE = new Tuto3MorePagesPlace();

	public Tuto3MorePagesPlace() {
		super((ViewProxy) GWT.create(Part3View.class), null);
	}

	@Override
	public MvpPlace getPlace(String token) {
		return Tuto3MorePagesPlace.INSTANCE;
	}
}
