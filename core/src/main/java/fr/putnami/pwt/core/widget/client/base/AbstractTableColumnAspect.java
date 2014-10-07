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
package fr.putnami.pwt.core.widget.client.base;

import com.google.gwt.user.client.ui.IsWidget;

import java.util.List;

import fr.putnami.pwt.core.editor.client.Context;
import fr.putnami.pwt.core.editor.client.Editor;
import fr.putnami.pwt.core.editor.client.Visitor;
import fr.putnami.pwt.core.model.client.ModelDriver;
import fr.putnami.pwt.core.model.client.base.HasDriver;

public abstract class AbstractTableColumnAspect<T>
	implements IsWidget, Editor, HasDriver<List<T>, ModelDriver<List<T>>>, Visitor {

	private String columnPath;
	private ModelDriver<List<T>> driver;

	private String path;

	public AbstractTableColumnAspect() {
	}

	protected AbstractTableColumnAspect(AbstractTableColumnAspect<T> source) {
		this.path = source.path;
		this.columnPath = source.columnPath;
	}

	@Override
	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public String getPath() {
		return this.path;
	}

	public String getColumnPath() {
		return this.columnPath;
	}

	public void setColumnPath(String columnPath) {
		this.columnPath = columnPath;
	}

	@Override
	public ModelDriver<List<T>> getDriver() {
		return this.driver;
	}

	@Override
	public void setDriver(ModelDriver<List<T>> driver) {
		driver.registerVisitor(this);
		this.driver = driver;
	}

	@Override
	public VisitorTrigger trigerOn() {
		return VisitorTrigger.BEFORE_EDIT;
	}

	@Override
	public <A, B extends Editor> boolean beforeVisit() {
		return true;
	}

	@Override
	public <A, B extends Editor> boolean visit(Context<B> context) {
		return true;
	}

	@Override
	public <A, B extends Editor> boolean afterVisit() {
		return true;
	}

	public abstract AbstractTableColumnAspect<T> cloneAspect();

}
