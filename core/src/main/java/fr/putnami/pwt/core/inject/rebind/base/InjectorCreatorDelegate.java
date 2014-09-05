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
package fr.putnami.pwt.core.inject.rebind.base;


public abstract class InjectorCreatorDelegate {
	public static final int HIGHEST_PRECEDENCE = Integer.MIN_VALUE;
	public static final int DEFAULT_PRECEDENCE = 0;
	public static final int LOWEST_PRECEDENCE = Integer.MAX_VALUE;

	public int getOrder() {
		return DEFAULT_PRECEDENCE;
	}
}
